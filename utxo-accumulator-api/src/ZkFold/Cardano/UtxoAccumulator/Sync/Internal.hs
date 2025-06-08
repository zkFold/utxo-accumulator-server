module ZkFold.Cardano.UtxoAccumulator.Sync.Internal where


import GeniusYield.Types
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (blake2b_224, byteStringToInteger)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorRedeemer (..))
import ZkFold.Cardano.UtxoAccumulator.Sync.Cache (cacheRestore, cacheUpdate)
import ZkFold.Cardano.UtxoAccumulator.Sync.FetchTx (fetchTx, FetchTxResult (..))
import ZkFold.Cardano.UtxoAccumulator.Types.Sync (SyncParams (..))

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
  mCache <- cacheRestore syncStateRef
  case mCache of
    Just cache -> return cache
    Nothing -> do
      maybeTxInfo <- trySync sp
      case maybeTxInfo of
        Just (ref', redeemer) -> do
          (hs, as) <- fullSyncInternal sp {syncStateRef = ref'}
          case redeemer of
            AddUtxo h _ -> return (hs ++ [toZp h], as)
            RemoveUtxo addr l _ _ -> return (hs, as ++ [toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData (addr, l))])
        Nothing -> return ([], [])

fullSync :: SyncParams -> IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
fullSync sp@SyncParams {..} = do
  result <- fullSyncInternal sp
  cacheUpdate syncStateRef result
  return result
