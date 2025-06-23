module ZkFold.Cardano.UtxoAccumulator.Utils where

import Data.Aeson (Value)
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GeniusYield.Imports (decodeUtf8Lenient, (>>>))
import GeniusYield.Types (GYOutDatum (..), GYTxOut (..), GYUTxO (..), PlutusVersion (..), datumToPlutus', mkGYTxOut)
import Numeric (showHex)
import PlutusLedgerApi.V3 (UnsafeFromData (..))
import ZkFold.Algebra.Number (Natural)
import Prelude

utxoToTxOut :: GYUTxO -> Maybe (GYTxOut 'PlutusV3)
utxoToTxOut GYUTxO {utxoAddress, utxoValue, utxoOutDatum} = do
  utxoDatum <- case utxoOutDatum of
    GYOutDatumInline d -> Just d
    _ -> Nothing
  return $ mkGYTxOut utxoAddress utxoValue utxoDatum

txOutToDatum :: UnsafeFromData a => GYTxOut 'PlutusV3 -> a
txOutToDatum GYTxOut {gyTxOutDatum} = case gyTxOutDatum of
  Just (d, _) -> unsafeFromBuiltinData $ datumToPlutus' d
  Nothing -> error "No datum found"

readHexToNatural :: T.Text -> Maybe Natural
readHexToNatural t = case TR.hexadecimal t of
  Right (i, rest) | T.null rest && i >= 0 -> Just (fromIntegral (i :: Integer))
  _ -> Nothing

parseNatural :: Value -> Parser Natural
parseNatural (Aeson.String t) =
  case T.stripPrefix "0x" t of
    Just hex ->
      case readHexToNatural hex of
        Just n -> pure n
        Nothing -> fail $ "Invalid hex string for Natural: " ++ T.unpack t
    Nothing -> fail $ "Expected hex string with 0x prefix for Natural, got: " ++ T.unpack t
parseNatural v = fail $ "Expected hex string for Natural, got: " ++ show v

-- | Convert a Natural to a 0x-prefixed, 56-digit hex string (for BLS12-381 scalar nonces)
writeNaturalToHex :: Natural -> T.Text
writeNaturalToHex n =
  let hex = T.pack (showHex n "")
      padded = T.justifyRight 56 '0' hex
   in T.append "0x" padded

bytestringToString :: ByteString -> String
bytestringToString = decodeUtf8Lenient >>> T.unpack