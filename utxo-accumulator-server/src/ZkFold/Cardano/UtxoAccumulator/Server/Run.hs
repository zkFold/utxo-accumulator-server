module ZkFold.Cardano.UtxoAccumulator.Server.Run (
  runServer,
  Mode (..),
) where

import Control.Exception (Exception (..), SomeException, try)
import Control.Monad.Except (ExceptT (..))
import Data.ByteString qualified as B
import Data.Maybe (fromJust, isNothing)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
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
import ZkFold.Cardano.UtxoAccumulator.Server.Api
import ZkFold.Cardano.UtxoAccumulator.Server.Auth
import ZkFold.Cardano.UtxoAccumulator.Server.Config (ServerConfig (..), coreConfigFromServerConfig, serverConfigOptionalFPIO, signingKeysFromServerWallet, updateConfigYaml)
import ZkFold.Cardano.UtxoAccumulator.Server.ErrorMiddleware
import ZkFold.Cardano.UtxoAccumulator.Server.RequestLoggerMiddleware (gcpReqLogger)
import ZkFold.Cardano.UtxoAccumulator.Server.Utils
import ZkFold.Cardano.UtxoAccumulator.TxBuilder (initAccumulatorRun, postScriptRun, removeUtxoRun)
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))

data Mode = ModeAccumulate | ModeDistribute Bool
  deriving (Eq, Show)

runServer :: Maybe FilePath -> Mode -> IO ()
runServer mfp mode = do
  sc@ServerConfig {..} <- serverConfigOptionalFPIO mfp
  (serverPaymentKey, serverStakeKey, serverAddr) <- fromJust <$> signingKeysFromServerWallet scNetworkId scWallet
  let coreCfg = coreConfigFromServerConfig sc
  withCfgProviders coreCfg "server" $ \providers -> do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
    logInfoS $ "UTxO Accumulator server version: " ++ showVersion PackageInfo.version
    logInfoS $ "Database file: " ++ scDatabasePath
    B.writeFile "web/openapi/api.yaml" (Yaml.encodePretty Yaml.defConfig utxoAccumulatorAPIOpenApi)
    reqLoggerMiddleware <- gcpReqLogger
    let
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
          & Warp.setPort scPort
          & Warp.setOnException onException
          & Warp.setOnExceptionResponse onExceptionResponse
      errLoggerMiddleware = errorLoggerMiddleware $ logErrorS . LT.unpack
      cfg =
        Config
          { cfgNetworkId = scNetworkId
          , cfgProviders = providers
          , cfgMaestroToken = let Confidential t = cpiMaestroToken scCoreProvider in T.unpack t
          , cfgPaymentKey = serverPaymentKey
          , cfgStakeKey = Just serverStakeKey
          , cfgAddress = serverAddr
          , cfgDatabasePath = scDatabasePath
          , cfgAccumulationValue = valueFromLovelace scAccumulationValue
          , cfgMaybeScriptRef = scMaybeScriptRef
          , cfgMaybeThreadTokenRef = scMaybeThreadTokenRef
          }

    crs <- utxoAccumulatorCRS

    -- Checking if the script is posted
    cfg' <-
      if isNothing scMaybeScriptRef
        then do
          logInfoS "Posting the UTxO Accumulator script..."
          postScriptRun cfg
        else return cfg

    -- Checking if the accumulator is initialized
    cfg'' <-
      if isNothing scMaybeThreadTokenRef
        then do
          logInfoS "Initializing the UTxO Accumulator..."
          initAccumulatorRun crs cfg'
        else return cfg'

    -- Update config.yaml with maybeScriptRef and maybeThreadTokenRef values
    updateConfigYaml "config.yaml" (cfgMaybeScriptRef cfg') (cfgMaybeThreadTokenRef cfg'')

    case mode of
      ModeAccumulate -> do
        logInfoS $ "Starting UTxO Accumulator server on port " +| scPort |+ "\nCore config:\n" +| indentF 4 (fromString $ show coreCfg) |+ ""
        let corsPolicy =
              simpleCorsResourcePolicy
                { corsRequestHeaders = ["content-type", "api-key"]
                , corsMethods = ["GET", "POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
        Warp.runSettings settings
          . cors (const $ Just corsPolicy)
          . reqLoggerMiddleware
          . errLoggerMiddleware
          . errorJsonWrapMiddleware
          $ let context = apiKeyAuthHandler (case scServerApiKey of Confidential t -> apiKeyFromText t) :. EmptyContext
             in serveWithContext mainAPI context
                  $ hoistServerWithContext
                    mainAPI
                    (Proxy :: Proxy '[AuthHandler Wai.Request ()])
                    (\ioAct -> Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try ioAct)
                  $ mainServer cfg''
      ModeDistribute removeNoDate ->
        removeUtxoRun crs cfg'' removeNoDate
