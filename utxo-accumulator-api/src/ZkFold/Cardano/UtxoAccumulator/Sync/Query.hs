module ZkFold.Cardano.UtxoAccumulator.Sync.Query where

import Data.Maybe (catMaybes, fromJust)
import GeniusYield.TxBuilder (GYTxQueryMonad (..))
import GeniusYield.Types
import ZkFold.Cardano.UtxoAccumulator.Constants (threadToken)
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config (..))
import ZkFold.Cardano.UtxoAccumulator.Types.Sync (SyncParams (..))

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

getSyncParams ::
  GYTxQueryMonad m =>
  Config ->
  [GYTxOutRef] ->
  m [SyncParams]
getSyncParams Config {..} refs = do
  let getParams ref = do
        stateRef <- getState (threadToken ref)
        case stateRef of
          Just sr ->
            return $
              Just $
                SyncParams
                  { syncStateRef = sr
                  , syncThreadToken = threadToken ref
                  , syncMaestroToken = cfgMaestroToken
                  , syncgNetworkId = cfgNetworkId
                  , syncCachePath = cfgCachePath
                  }
          Nothing -> return Nothing
  catMaybes <$> mapM getParams refs
