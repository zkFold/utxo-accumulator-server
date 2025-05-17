module ZkFold.Cardano.UtxoAccumulator.Utils where

import GeniusYield.Types (GYOutDatum (..), GYTxOut (..), GYUTxO (..), PlutusVersion (..), datumToPlutus', mkGYTxOut)
import PlutusLedgerApi.V3 (UnsafeFromData (..))
import PlutusTx.Prelude (Maybe (..), ($))
import Prelude (error, return)

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
