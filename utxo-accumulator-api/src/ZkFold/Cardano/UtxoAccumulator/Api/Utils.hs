module ZkFold.Cardano.UtxoAccumulator.Api.Utils where

import GeniusYield.TxBuilder (GYTxQueryMonad (..), MonadError (..), GYTxMonadException (..))
import GeniusYield.Types
import PlutusLedgerApi.V3 (unsafeFromBuiltinData)
import ZkFold.Cardano.UtxoAccumulator.Types.State (UtxoAccumulatorState)
import Data.Function ((&))
import Data.List (find)

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

findCollateral :: GYTxQueryMonad m => GYAddress -> m GYUTxO
findCollateral addr = do
  utxos <- utxosAtAddress addr Nothing
  case utxosToList utxos & reverse & find (\GYUTxO {..} ->
    valueNonAda utxoValue == mempty && valueAtleast5Ada utxoValue) of
      Nothing -> throwError $ GYNoSuitableCollateralException 5_000_000 addr
      Just utxo -> pure utxo
 where
  valueAtleast5Ada :: GYValue -> Bool
  valueAtleast5Ada val = valueAda val >= 5_000_000
