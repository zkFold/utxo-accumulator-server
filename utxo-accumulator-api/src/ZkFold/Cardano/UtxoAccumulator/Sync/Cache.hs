module ZkFold.Cardano.UtxoAccumulator.Sync.Cache where

import Data.Map (Map, fromList, insert, lookup)
import GeniusYield.Types
import System.Directory (doesFileExist)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Orphans ()
import ZkFold.Prelude (readFileJSON, writeFileJSON)
import Prelude hiding (lookup)

type Cache = Map GYTxOutRef ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])

cacheRestore :: FilePath -> GYTxOutRef -> IO (Maybe ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point]))
cacheRestore cacheFile ref = do
  cacheExists <- doesFileExist cacheFile
  if cacheExists
    then lookup ref <$> readFileJSON @Cache cacheFile
    else return Nothing

cacheUpdate :: FilePath -> GYTxOutRef -> ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point]) -> IO ()
cacheUpdate cacheFile ref (hs, as) = do
  cacheExists <- doesFileExist cacheFile
  if cacheExists
    then do
      cache <- readFileJSON @Cache cacheFile
      let updatedCache = insert ref (hs, as) cache
      writeFileJSON cacheFile updatedCache
    else do
      let initialCache = fromList [(ref, (hs, as))]
      writeFileJSON cacheFile initialCache
