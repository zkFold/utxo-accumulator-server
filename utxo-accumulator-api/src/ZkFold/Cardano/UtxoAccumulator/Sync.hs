module ZkFold.Cardano.UtxoAccumulator.Sync where

import Data.Bool (bool)
import Data.Map (Map, elems, filterWithKey, findMin, fromList)
import GeniusYield.Types
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (BuiltinByteString, blake2b_224, byteStringToInteger, indexByteString, sliceByteString)
import System.Directory (doesFileExist)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Prelude (readFileJSON, writeFileJSON)

addressFromTicket :: BuiltinByteString -> (Natural, ScalarFieldOf BLS12_381_G1_Point)
addressFromTicket bs =
  let i0 = indexByteString bs 0
      i1 = indexByteString bs 1
      i = fromIntegral $ i1 * 256 + i0
      bs' = sliceByteString 2 28 bs
   in (i, toZp $ byteStringToInteger BigEndian bs')

addressList :: [BuiltinByteString] -> [ScalarFieldOf BLS12_381_G1_Point]
addressList = elems . fromList . map addressFromTicket

-- TODO: Implement this
getTickets :: Monad m => m [BuiltinByteString]
getTickets = return []

findUnusedTicket :: Map GYAddress (ScalarFieldOf BLS12_381_G1_Point) -> [ScalarFieldOf BLS12_381_G1_Point] -> Maybe (GYAddress, ScalarFieldOf BLS12_381_G1_Point)
findUnusedTicket m as =
  let m' = filterWithKey (\k _ -> toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData $ addressToPlutus k) `notElem` as) m
   in bool Nothing (Just $ findMin m') (not $ null m')

getTxs :: IO [(GYAddress, ScalarFieldOf BLS12_381_G1_Point)]
getTxs = do
  b <- doesFileExist "txs.json"
  if b
    then readFileJSON "txs.json"
    else return mempty

putTxs :: [(GYAddress, ScalarFieldOf BLS12_381_G1_Point)] -> IO ()
putTxs = writeFileJSON "txs.json"
