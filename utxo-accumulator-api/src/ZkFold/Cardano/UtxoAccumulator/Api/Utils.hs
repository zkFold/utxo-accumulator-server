module ZkFold.Cardano.UtxoAccumulator.Api.Utils where

import GeniusYield.TxBuilder (GYTxQueryMonad (..))
import GeniusYield.Types
import PlutusLedgerApi.V3 (unsafeFromBuiltinData)
import ZkFold.Cardano.UtxoAccumulator.Types.State (UtxoAccumulatorState)

getState ::
  (GYTxQueryMonad m) =>
  GYAssetClass ->
  m UtxoAccumulatorState
getState _ = do
  -- TODO: finds a UTXO with a token
  utxo <- undefined
  return $ case utxoOutDatum utxo of
    GYOutDatumInline d -> unsafeFromBuiltinData $ datumToPlutus' d
    _ -> Nothing

getOutput ::
  (GYTxQueryMonad m) =>
  GYTxOutRef ->
  m (Maybe (GYTxOut 'PlutusV3))
getOutput ref = do
  utxo <- utxoAtTxOutRef ref
  return $ case utxo of
    Just (GYUTxO _ utxoAddress utxoValue (GYOutDatumInline utxoDatum) _) ->
      Just $ mkGYTxOut utxoAddress utxoValue utxoDatum
    _ -> Nothing
