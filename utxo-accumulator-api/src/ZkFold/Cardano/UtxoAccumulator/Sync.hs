module ZkFold.Cardano.UtxoAccumulator.Sync where

import Data.Aeson (FromJSON, decode, (.:))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Simple

import Codec.Serialise (deserialise)
import Data.Aeson.Types (Parser)
import Data.ByteString (fromStrict)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BSC
import Data.Either (partitionEithers)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import GeniusYield.Providers (utxoFromMaestro)
import GeniusYield.Types
import Maestro.Types.V1 (UtxoWithBytes)
import PlutusLedgerApi.V3 (Data, FromData (fromBuiltinData), dataToBuiltinData, toBuiltinData)
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (blake2b_224, byteStringToInteger)
import System.Directory (doesFileExist)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorRedeemer (..))
import ZkFold.Cardano.UtxoAccumulator.Constants (threadToken)
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))
import ZkFold.Prelude (writeFileJSON, readFileJSON)

data FetchTxResult = FetchTxResult UtxoAccumulatorRedeemer [GYUTxO]
  deriving Show

instance FromJSON FetchTxResult where
  parseJSON = Aeson.withObject "FetchTxResult" $ \v -> do
    dataField <- v .: "data"
    redeemersField <- dataField .: "redeemers"
    spendsField <- redeemersField .: "spends"
    dataField' <-
      if V.null spendsField
        then fail "No spending redeemers"
        else V.head spendsField .: "data"
    bytesField <- dataField' .: "bytes"
    inputsField <- dataField .: "inputs" :: Parser [UtxoWithBytes]
    let inputs = snd $ partitionEithers $ map utxoFromMaestro inputsField
    case bytesField of
      Aeson.String redeemerStr ->
        case B16.decode (BSC.pack $ T.unpack redeemerStr) of
          Left err -> fail $ "Failed to decode redeemer bytes: " ++ err
          Right bs -> case fromBuiltinData $ dataToBuiltinData $ deserialise @Data $ fromStrict bs of
            Just redeemer -> return $ FetchTxResult redeemer inputs
            Nothing -> fail "Failed to parse redeemer from bytes"
      _ -> fail "Expected a string for the redeemer bytes"

fetchTx ::
  -- | Network ID
  String ->
  -- | API Key
  String ->
  -- | Transaction Hash
  T.Text ->
  IO (Maybe FetchTxResult)
fetchTx nId apiKey txHash = do
  let url = "https://" ++ nId ++ ".gomaestro-api.org/v1/transactions/" ++ T.unpack txHash
      request =
        setRequestHeader "Accept" ["application/json"] $
          setRequestHeader "api-key" [TE.encodeUtf8 (T.pack apiKey)] $
            parseRequest_ url
  response <- httpLBS request
  let body = getResponseBody response
  return $ decode body

-- | Path to the cache file
cacheFile :: FilePath
cacheFile = "database/cache.json"

type Cache = (GYTxOutRef, ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point]))

cacheHit :: GYTxOutRef -> IO Bool
cacheHit ref = do
  cacheExists <- doesFileExist cacheFile
  if cacheExists
    then do
      (lastRef, _) <- readFileJSON @Cache cacheFile
      return $ lastRef == ref
    else return False

cacheRestore :: IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
cacheRestore = do
  cacheExists <- doesFileExist cacheFile
  if cacheExists
    then do
      (_, (hs, as)) <- readFileJSON @Cache cacheFile
      return (hs, as)
    else return ([], [])

cacheUpdate :: GYTxOutRef -> ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point]) -> IO ()
cacheUpdate ref (hs, as) = do
  let cacheData = (ref, (hs, as))
  writeFileJSON cacheFile cacheData

trySync :: Config -> GYTxOutRef -> IO (Maybe (GYTxOutRef, UtxoAccumulatorRedeemer))
trySync Config {..} ref = do
  let nId = if cfgNetworkId == GYMainnet then "mainnet" else "preprod"
  maybeRes <- fetchTx nId cfgMaestroToken (fst $ txOutRefToTuple' ref)
  case maybeRes of
    Just (FetchTxResult red utxos) -> do
      let f x = valueAssetClass (utxoValue x) (threadToken $ fromJust cfgMaybeThreadTokenRef) > 0
          utxo = head $ filter f utxos
      return $ Just (utxoRef utxo, red)
    _ -> return Nothing

fullSyncInternal :: Config -> GYTxOutRef -> IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
fullSyncInternal cfg ref = do
  b <- cacheHit ref
  if b
    then cacheRestore
    else do
      maybeTxInfo <- trySync cfg ref
      case maybeTxInfo of
        Just (ref', redeemer) -> do
          (hs, as) <- fullSyncInternal cfg ref'
          case redeemer of
            AddUtxo h _ -> return (hs ++ [toZp h], as)
            RemoveUtxo addr l _ _ -> return (hs, as ++ [toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData (addr, l))])
        Nothing -> return ([], [])

fullSync :: Config -> GYTxOutRef -> IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
fullSync cfg ref = do
  result <- fullSyncInternal cfg ref
  cacheUpdate ref result
  return result
