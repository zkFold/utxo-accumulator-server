module ZkFold.Cardano.UtxoAccumulator.Sync where

import Data.Aeson (FromJSON, decode, (.:))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Simple

import Data.Aeson.Types (Parser)
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Map (Map, filterWithKey, findMin)
import Data.Vector qualified as V
import GeniusYield.Providers (utxoFromMaestro)
import GeniusYield.Types
import Maestro.Types.V1 (UtxoWithBytes)
import PlutusLedgerApi.V3 (toBuiltinData, unsafeFromBuiltinData)
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (blake2b_224, byteStringToInteger)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorRedeemer (..))
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))

data FetchTxResult = FetchTxResult String [GYUTxO]
  deriving Show

instance FromJSON FetchTxResult where
  parseJSON = Aeson.withObject "FetchTxResult" $ \v -> do
    dataField <- v .: "data"
    redeemersField <- dataField .: "redeemers"
    spendsField <- redeemersField .: "spends"
    dataField' <- V.head spendsField .: "data"
    bytesField <- dataField' .: "bytes"
    inputsField <- dataField .: "inputs" :: Parser [UtxoWithBytes]
    let inputs = snd $ partitionEithers $ map utxoFromMaestro inputsField
    case bytesField of
      Aeson.String red -> return $ FetchTxResult (T.unpack red) inputs
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

trySync :: Config -> GYTxOutRef -> IO (Maybe (GYTxOutRef, UtxoAccumulatorRedeemer))
trySync Config {..} ref = do
  let nId = if cfgNetworkId == GYMainnet then "mainnet" else "preprod"
  maybeRes <- fetchTx nId cfgMaestroToken (fst $ txOutRefToTuple' ref)
  case maybeRes of
    Just _ -> do
      let red = AddUtxo 0 (unsafeFromBuiltinData $ toBuiltinData @Integer 0)
      return $ Just (ref, red)
    _ -> return Nothing

fullSyncInternal :: Config -> GYTxOutRef -> IO [UtxoAccumulatorRedeemer]
fullSyncInternal cfg ref = do
  maybeTxInfo <- trySync cfg ref
  case maybeTxInfo of
    Just (ref', redeemer) -> do
      redeemers <- fullSyncInternal cfg ref'
      return (redeemers ++ [redeemer])
    Nothing -> return []

fullSync :: Config -> GYTxOutRef -> IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
fullSync cfg ref = do
  redeemers <- fullSyncInternal cfg ref
  return $
    foldl
      ( \(hs, as) redeemer -> case redeemer of
          AddUtxo h _ -> (hs ++ [toZp h], as)
          RemoveUtxo addr _ _ -> (hs, toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr) : as)
      )
      ([], [])
      redeemers

findUnusedTransactionData :: Map GYAddress (ScalarFieldOf BLS12_381_G1_Point) -> [ScalarFieldOf BLS12_381_G1_Point] -> Maybe (GYAddress, ScalarFieldOf BLS12_381_G1_Point)
findUnusedTransactionData m as =
  let m' = filterWithKey (\k _ -> toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData $ addressToPlutus k) `notElem` as) m
   in bool Nothing (Just $ findMin m') (not $ null m')
