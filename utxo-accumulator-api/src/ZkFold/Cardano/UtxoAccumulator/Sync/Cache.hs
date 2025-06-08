module ZkFold.Cardano.UtxoAccumulator.Sync.Cache where

import Data.Map (Map, lookup)
import GeniusYield.Types
import System.Directory (doesFileExist)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Orphans ()
import ZkFold.Prelude (readFileJSON, writeFileJSON)
import Prelude hiding (lookup)

-- | Path to the cache file
cacheFile :: FilePath
cacheFile = "database/cache.json"

type Cache = Map GYTxOutRef ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point])

cacheRestore :: GYTxOutRef -> IO (Maybe ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point]))
cacheRestore ref = do
  cacheExists <- doesFileExist cacheFile
  if cacheExists
    then lookup ref <$> readFileJSON @Cache cacheFile
    else return Nothing

cacheUpdate :: GYTxOutRef -> ([ScalarFieldOf BLS12_381_G1_Point], [ScalarFieldOf BLS12_381_G1_Point]) -> IO ()
cacheUpdate ref (hs, as) = do
  let cacheData = (ref, (hs, as))
  writeFileJSON cacheFile cacheData
