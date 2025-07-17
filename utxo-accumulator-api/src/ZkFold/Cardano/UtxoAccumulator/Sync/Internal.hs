module ZkFold.Cardano.UtxoAccumulator.Sync.Internal where

import Data.List (maximumBy)
import Data.Map (fromList, singleton, toList)
import Data.Maybe (fromJust)
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Algebra.Number (value)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorRedeemer (..))
import ZkFold.Cardano.UtxoAccumulator.Constants (N)
import ZkFold.Cardano.UtxoAccumulator.IO (runQueryWithConfig)
import ZkFold.Cardano.UtxoAccumulator.Sync.Cache (Cache, cacheRestore, cacheUpdate)
import ZkFold.Cardano.UtxoAccumulator.Sync.FetchTx (FetchTxResult (..), fetchTx)
import ZkFold.Cardano.UtxoAccumulator.Sync.Query (getSyncParams)
import ZkFold.Cardano.UtxoAccumulator.Transition (utxoAccumulatorAddressHash)
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config (..))
import ZkFold.Cardano.UtxoAccumulator.Types.Sync (SyncParams (..))
import ZkFold.Prelude (length)
import Prelude hiding (length)

trySync :: SyncParams -> IO (Maybe (GYTxOutRef, UtxoAccumulatorRedeemer))
trySync SyncParams {..} = do
  let nId = if syncgNetworkId == GYMainnet then "mainnet" else "preprod"
  maybeRes <- fetchTx nId syncMaestroToken (fst $ txOutRefToTuple' syncStateRef)
  case maybeRes of
    Just (FetchTxResult red utxos) -> do
      let f x = valueAssetClass (utxoValue x) syncThreadToken > 0
          utxo = head $ filter f utxos
      return $ Just (utxoRef utxo, red)
    _ -> return Nothing

fullSyncInternal :: SyncParams -> IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
fullSyncInternal sp@SyncParams {..} = do
  mCache <- cacheRestore syncCachePath syncStateRef
  case mCache of
    Just cache -> return cache
    Nothing -> do
      maybeTxInfo <- trySync sp
      case maybeTxInfo of
        Just (ref', redeemer) -> do
          (hs, as) <- fullSyncInternal sp {syncStateRef = ref'}
          case redeemer of
            AddUtxo h _ -> return (hs ++ [toZp h], as)
            RemoveUtxo addr l _ _ -> return (hs, as ++ [utxoAccumulatorAddressHash addr (toZp l)])
        Nothing -> return ([], [])

fullSync :: SyncParams -> IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
fullSync sp@SyncParams {..} = do
  result <- fullSyncInternal sp
  cacheUpdate syncCachePath syncStateRef result
  return result

fullSyncFromConfig :: Config -> IO Cache
fullSyncFromConfig cfg@Config {..} = do
  case cfgNetworkId of
    GYPrivnet _ -> singleton (head cfgThreadTokenRefs) . fromJust <$> cacheRestore cfgCachePath (head cfgThreadTokenRefs)
    _ -> do
      syncParamsList <- zip cfgThreadTokenRefs <$> runQueryWithConfig cfg (getSyncParams cfg cfgThreadTokenRefs)
      fromList <$> mapM (\(ref, sp) -> do r <- fullSync sp; return (ref, r)) syncParamsList

threadTokenRefFromSync :: Config -> IO (Maybe GYTxOutRef)
threadTokenRefFromSync cfg = do
  results <- toList <$> fullSyncFromConfig cfg
  let filtered = filter ((< value @N) . length . fst . snd) results
  return $
    if null filtered
      then Nothing
      else Just $ fst $ maximumBy (\(_, (hs1, _)) (_, (hs2, _)) -> compare (length hs1) (length hs2)) filtered
