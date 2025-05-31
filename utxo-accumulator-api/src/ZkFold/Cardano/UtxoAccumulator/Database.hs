module ZkFold.Cardano.UtxoAccumulator.Database where

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

removeUtxoAccumulatorData :: FilePath -> IO ()
removeUtxoAccumulatorData = removeFile

getUtxoAccumulatorData :: FilePath -> IO (Map GYAddress (ScalarFieldOf BLS12_381_G1_Point))
getUtxoAccumulatorData fp = do
  b <- doesFileExist fp
  if b
    then fromList <$> readFileJSON fp
    else return mempty

putUtxoAccumulatorData :: FilePath -> Map GYAddress (ScalarFieldOf BLS12_381_G1_Point) -> IO ()
putUtxoAccumulatorData fp = writeFileJSON fp . toList
