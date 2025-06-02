module ZkFold.Cardano.UtxoAccumulator.Database where

import Control.Monad (when)
import Data.Map (Map, fromList, toList)
import GeniusYield.Types
import System.Directory (doesFileExist, removeFile)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Prelude (readFileJSON, writeFileJSON)

removeUtxoAccumulatorData :: FilePath -> IO ()
removeUtxoAccumulatorData fp = do
  b <- doesFileExist fp
  when b $ removeFile fp

getUtxoAccumulatorData :: FilePath -> IO (Map GYAddress (ScalarFieldOf BLS12_381_G1_Point))
getUtxoAccumulatorData fp = do
  b <- doesFileExist fp
  if b
    then fromList <$> readFileJSON fp
    else return mempty

putUtxoAccumulatorData :: FilePath -> Map GYAddress (ScalarFieldOf BLS12_381_G1_Point) -> IO ()
putUtxoAccumulatorData fp = writeFileJSON fp . toList
