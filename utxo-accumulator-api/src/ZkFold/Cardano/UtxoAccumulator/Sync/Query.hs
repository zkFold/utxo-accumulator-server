module ZkFold.Cardano.UtxoAccumulator.Sync.Query where

import Data.Maybe (fromJust)
import GeniusYield.TxBuilder (GYTxQueryMonad (..))
import GeniusYield.Types

getState ::
  GYTxQueryMonad m =>
  GYAssetClass ->
  m (Maybe GYTxOutRef)
getState tt = do
  utxos <- utxosWithAsset (fromJust $ nonAdaTokenFromAssetClass tt)
  return $ case utxosToList utxos of
    GYUTxO {utxoRef} : _ -> Just utxoRef
    [] -> Nothing

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
