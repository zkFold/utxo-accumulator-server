module ZkFold.Cardano.UtxoAccumulator.Server.Run (
  runServer,
  postScript,
  initAccumulator,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Thread.Delay (delay)
import Control.Exception (Exception (..), SomeException, try)
import Control.Monad.Except (ExceptT (..))
import Data.ByteString qualified as B
import Data.Maybe (fromJust, isNothing)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Data.Yaml.Pretty qualified as Yaml
import Fmt
import GeniusYield.GYConfig
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import PackageInfo_utxo_accumulator_server qualified as PackageInfo
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Internal.ServerError (responseServerError)
import System.TimeManager (TimeoutThread (..))
import ZkFold.Cardano.UtxoAccumulator.Constants (utxoAccumulatorCRS)
import ZkFold.Cardano.UtxoAccumulator.Database (cleanUtxoAccumulatorData)
import ZkFold.Cardano.UtxoAccumulator.Server.Api
import ZkFold.Cardano.UtxoAccumulator.Server.Auth
import ZkFold.Cardano.UtxoAccumulator.Server.Config (ServerConfig (..), coreConfigFromServerConfig, serverConfigOptionalFPIO, signingKeysFromServerWallet, updateConfigYaml)
import ZkFold.Cardano.UtxoAccumulator.Server.ErrorMiddleware
import ZkFold.Cardano.UtxoAccumulator.Server.Git (createConfigUpdatePR)
import ZkFold.Cardano.UtxoAccumulator.Server.Options (InitAccumulatorOptions (..), PostScriptOptions (..), ServerOptions (..))
import ZkFold.Cardano.UtxoAccumulator.Server.RSA (RSAKeyPair, generateRSAKeyPair)
import ZkFold.Cardano.UtxoAccumulator.Server.RequestLoggerMiddleware (gcpReqLogger)
import ZkFold.Cardano.UtxoAccumulator.Server.Utils
import ZkFold.Cardano.UtxoAccumulator.Sync (threadTokenRefFromSync)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder (initAccumulatorRun, postScriptRun, removeUtxoRun)
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)

-- | Helper function to set up configuration and providers
setupConfigAndProviders :: FilePath -> (Config -> (String -> IO ()) -> (String -> IO ()) -> ServerConfig -> IO a) -> IO a
setupConfigAndProviders configPath action = do
  sc@ServerConfig {..} <- serverConfigOptionalFPIO (Just configPath)
  (serverPaymentKey, serverStakeKey, serverAddr) <- fromJust <$> signingKeysFromServerWallet scNetworkId scWallet
  let coreCfg = coreConfigFromServerConfig sc
  withCfgProviders coreCfg "utxo-accumulator" $ \providers -> do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
        cfg =
          Config
            { cfgNetworkId = scNetworkId
            , cfgProviders = providers
            , cfgMaestroToken = let Confidential t = cpiMaestroToken scCoreProvider in T.unpack t
            , cfgPaymentKey = serverPaymentKey
            , cfgStakeKey = Just serverStakeKey
            , cfgAddress = serverAddr
            , cfgDatabasePath = scDatabasePath
            , cfgCachePath = scCachePath
            , cfgAccumulationValue = scAccumulationValue
            , cfgMaybeScriptRef = scMaybeScriptRef
            , cfgThreadTokenRefs = scThreadTokenRefs
            }
    action cfg logInfoS logErrorS sc

postScript :: PostScriptOptions -> IO ()
postScript PostScriptOptions {psoConfigPath} = setupConfigAndProviders psoConfigPath $ \cfg logInfoS _logErrorS _sc -> do
  cfg' <- postScriptRun cfg
  logInfoS "Accumulator script posted"

  -- Update config if needed and create PR when config is updated
  configUpdated <- updateConfigYaml psoConfigPath (cfgMaybeScriptRef cfg') (cfgThreadTokenRefs cfg')
  when configUpdated $ do
    logInfoS "Configuration file updated with new script reference"
    createConfigUpdatePR psoConfigPath logInfoS

initAccumulator :: InitAccumulatorOptions -> IO ()
initAccumulator InitAccumulatorOptions {..} = setupConfigAndProviders iaoConfigPath $ \cfg logInfoS _logErrorS _sc -> do
  logInfoS $ "UTxO Accumulator initialization command (creating " ++ show iaoNumTokens ++ " thread tokens)"

  logInfoS $ "Initializing the UTxO Accumulator with " ++ show iaoNumTokens ++ " thread tokens..."
  crs <- utxoAccumulatorCRS

  -- Call initAccumulatorRun exactly iaoNumTokens times
  finalCfg <-
    foldM
      ( \cfg' i -> do
          logInfoS $ "Creating thread token " ++ show i ++ "/" ++ show iaoNumTokens
          initAccumulatorRun crs cfg'
      )
      cfg
      [1 .. iaoNumTokens]

  -- Update config file with the new thread token references
  configUpdated <- updateConfigYaml iaoConfigPath (cfgMaybeScriptRef finalCfg) (cfgThreadTokenRefs finalCfg)

  -- Create PR if config was updated
  when configUpdated $ do
    logInfoS "Config file was updated with new thread token references"
    createConfigUpdatePR iaoConfigPath logInfoS

  logInfoS $ "Accumulator initialization completed successfully! Created " ++ show iaoNumTokens ++ " thread tokens."

runServer :: ServerOptions -> IO ()
runServer ServerOptions {..} = setupConfigAndProviders soConfigPath $ \cfg logInfoS logErrorS sc@ServerConfig {..} -> do
  rsaKeyPair <- generateRSAKeyPair

  logInfoS $ "UTxO Accumulator server version: " ++ showVersion PackageInfo.version
  logInfoS $ "Database file: " ++ scDatabasePath
  B.writeFile "web/openapi/api.yaml" (Yaml.encodePretty Yaml.defConfig utxoAccumulatorAPIOpenApi)

  -- Check if script is posted
  if isNothing scMaybeScriptRef
    then do
      logErrorS "ERROR: Script must be posted before running the server!"
      logErrorS "Please run: utxo-accumulator-server post-script --config <config-file>"
      error "Script not posted"
    else logInfoS "✓ Script is posted"

  -- Check if accumulator is initialized
  mRef <- threadTokenRefFromSync cfg
  if isNothing mRef
    then do
      logErrorS "ERROR: Accumulator must be initialized before running the server!"
      logErrorS "Please run: utxo-accumulator-server init --config <config-file>"
      error "Accumulator not initialized"
    else logInfoS "✓ Accumulator is initialized"

  ref <- fromJust <$> threadTokenRefFromSync cfg
  crs <- utxoAccumulatorCRS
  let coreCfg = coreConfigFromServerConfig sc

  -- Start the distribution thread
  _ <- forkIO $ distributionThread crs cfg soForceDistribute soCleanDb logInfoS scDatabasePath

  -- Start the accumulation thread (main thread)
  accumulationThread rsaKeyPair crs cfg ref scServerApiKey scPort coreCfg logInfoS logErrorS

accumulationThread :: RSAKeyPair -> UtxoAccumulatorCRS -> Config -> GYTxOutRef -> Confidential T.Text -> Int -> GYCoreConfig -> (String -> IO ()) -> (String -> IO ()) -> IO ()
accumulationThread rsaKeyPair crs cfg ref serverApiKey port coreCfg logInfoS logErrorS = do
  let corsPolicy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = ["content-type", "api-key"]
          , corsMethods = ["GET", "POST", "OPTIONS"]
          , corsOrigins = Nothing
          }
      -- These are only meant to catch fatal exceptions, application thrown exceptions should be caught beforehand.
      onException :: req -> SomeException -> IO ()
      onException _req exc =
        displayException exc
          & if isMatchedException exceptionsToIgnore exc
            then logInfoS
            else logErrorS
       where
        -- TimeoutThread and Warp.ConnectionClosedByPeer do not indicate that anything is wrong and
        -- should not be logged as errors. See
        -- https://magnus.therning.org/2021-07-03-the-timeout-manager-exception.html
        -- https://www.rfc-editor.org/rfc/rfc5246#page-29
        exceptionsToIgnore = Proxy @TimeoutThread :>> Proxy @Warp.InvalidRequest :>> ENil
      onExceptionResponse :: SomeException -> Wai.Response
      onExceptionResponse _ = responseServerError . apiErrorToServerError $ someBackendError "Internal Server Error"
      settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setOnException onException
          & Warp.setOnExceptionResponse onExceptionResponse
      errLoggerMiddleware = errorLoggerMiddleware $ logErrorS . LT.unpack
  reqLoggerMiddleware <- gcpReqLogger
  logInfoS $ "Starting UTxO Accumulator server on port " +| port |+ "\nCore config:\n" +| indentF 4 (fromString $ show coreCfg) |+ ""
  Warp.runSettings settings
    . cors (const $ Just corsPolicy)
    . reqLoggerMiddleware
    . errLoggerMiddleware
    . errorJsonWrapMiddleware
    $ let context = apiKeyAuthHandler (case serverApiKey of Confidential t -> apiKeyFromText t) :. EmptyContext
       in serveWithContext mainAPI context
            $ hoistServerWithContext
              mainAPI
              (Proxy :: Proxy '[AuthHandler Wai.Request ()])
              (\ioAct -> Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try ioAct)
            $ mainServer rsaKeyPair crs cfg ref

distributionThread :: UtxoAccumulatorCRS -> Config -> Bool -> Bool -> (String -> IO ()) -> FilePath -> IO ()
distributionThread crs cfg forceDist cleanDb logInfoS databasePath = do
  forM_ [1 :: Int ..] $ const $ do
    now <- getPOSIXTime
    removeUtxoRun crs cfg forceDist
    logInfoS "UTxO Accumulator server finished fund distribution."
    when cleanDb $ do
      logInfoS "Cleaning transaction database from old transactions and those with no timer..."
      removed <- cleanUtxoAccumulatorData databasePath now
      logInfoS $ "Removed " ++ show removed ++ " old/no-timer transactions from the database."
    delay 1200_000_000 -- Sleep for 20 minutes before the next iteration
