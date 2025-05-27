module ZkFold.Cardano.UtxoAccumulator.Api.Utils where

import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromJust)
import GeniusYield.TxBuilder (GYTxMonadException (..), GYTxQueryMonad (..), MonadError (..))
import GeniusYield.Types
import ZkFold.Cardano.UtxoAccumulator.Types.State (UtxoAccumulatorState)

getState ::
  GYTxQueryMonad m =>
  GYAssetClass ->
  m UtxoAccumulatorState
getState tt = do
  Just . head . utxosRefs <$> utxosWithAsset (fromJust $ nonAdaTokenFromAssetClass tt)

getOutput ::
  GYTxQueryMonad m =>
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
  case utxosToList utxos
    & reverse
    & find
      ( \GYUTxO {..} ->
          valueNonAda utxoValue == mempty && valueAtleast5Ada utxoValue
      ) of
    Nothing -> throwError $ GYNoSuitableCollateralException 5_000_000 addr
    Just utxo -> pure utxo
 where
  valueAtleast5Ada :: GYValue -> Bool
  valueAtleast5Ada val = valueAda val >= 5_000_000
