module ZkFold.Cardano.UtxoAccumulator.Sync.FetchTx where

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
import Data.Vector qualified as V
import GeniusYield.Providers (utxoFromMaestro)
import GeniusYield.Types
import Maestro.Types.V1 (UtxoWithBytes)
import PlutusLedgerApi.V3 (Data, FromData (fromBuiltinData), dataToBuiltinData)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorRedeemer (..))

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
