module ZkFold.Cardano.UtxoAccumulator.Sync where

import Data.Bool (bool)
import Data.Map (Map, filterWithKey, findMin, fromList, toList)
import GeniusYield.Types
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (blake2b_224, byteStringToInteger)
import System.Directory (doesFileExist, removeFile)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorRedeemer (..))
import ZkFold.Prelude (readFileJSON, writeFileJSON)

removeUtxoAccumulatorData :: IO ()
removeUtxoAccumulatorData = do
  removeFile "txs.json"

getUtxoAccumulatorData :: IO (Map GYAddress (ScalarFieldOf BLS12_381_G1_Point))
getUtxoAccumulatorData = do
  b <- doesFileExist "txs.json"
  if b
    then fromList <$> readFileJSON "txs.json"
    else return mempty

putUtxoAccumulatorData :: Map GYAddress (ScalarFieldOf BLS12_381_G1_Point) -> IO ()
putUtxoAccumulatorData = writeFileJSON "txs.json" . toList

trySync :: GYNetworkId -> GYTxOut 'PlutusV3 -> IO (Maybe (GYTxOut 'PlutusV3, UtxoAccumulatorRedeemer))
trySync _ _ = do
  return Nothing

fullSyncInternal :: GYNetworkId -> GYTxOut 'PlutusV3 -> IO [UtxoAccumulatorRedeemer]
fullSyncInternal nId txOut = do
  maybeTxOut' <- trySync nId txOut
  case maybeTxOut' of
    Just (txOut', redeemer) -> do
      redeemers <- fullSyncInternal nId txOut'
      return (redeemers ++ [redeemer])
    Nothing -> return []

fullSync :: GYNetworkId -> GYTxOut 'PlutusV3 -> IO ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])
fullSync nId txOut = do
  redeemers <- fullSyncInternal nId txOut
  return $
    foldl
      ( \(hs, as) redeemer -> case redeemer of
          AddUtxo h _ -> (hs ++ [toZp h], as)
          RemoveUtxo addr _ _ -> (hs, toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr) : as)
      )
      ([], [])
      redeemers

findUnusedTransactionData :: Map GYAddress (ScalarFieldOf BLS12_381_G1_Point) -> [ScalarFieldOf BLS12_381_G1_Point] -> Maybe (GYAddress, ScalarFieldOf BLS12_381_G1_Point)
findUnusedTransactionData m as =
  let m' = filterWithKey (\k _ -> toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData $ addressToPlutus k) `notElem` as) m
   in bool Nothing (Just $ findMin m') (not $ null m')
