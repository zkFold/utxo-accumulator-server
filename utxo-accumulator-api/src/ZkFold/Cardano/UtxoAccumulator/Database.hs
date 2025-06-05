module ZkFold.Cardano.UtxoAccumulator.Database where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map (Map, fromList, toList)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import GeniusYield.Types
import System.Directory (doesFileExist, removeFile)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Prelude (readFileJSON, writeFileJSON)

data AccumulatorDataKey = AccumulatorDataKey
  { adkAddress :: GYAddress
  , adkNonceL :: ScalarFieldOf BLS12_381_G1_Point
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

{- | Optional distribution/removal time for a UTxO
  (Nothing means no scheduled removal)
-}
type UtxoDistributionTime = Maybe POSIXTime

{- | Data item for each accumulated UTxO
  (right nonce and optional distribution/removal time)
-}
data AccumulatorDataItem = AccumulatorDataItem
  { adiNonceR :: ScalarFieldOf BLS12_381_G1_Point
  , adiDistributionTime :: UtxoDistributionTime
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The accumulator state: address -> data item
type AccumulatorData = Map AccumulatorDataKey AccumulatorDataItem

removeUtxoAccumulatorData :: FilePath -> IO ()
removeUtxoAccumulatorData fp = do
  b <- doesFileExist fp
  when b $ removeFile fp

getUtxoAccumulatorData :: FilePath -> IO AccumulatorData
getUtxoAccumulatorData fp = do
  b <- doesFileExist fp
  if b
    then fromList <$> readFileJSON fp
    else return mempty

putUtxoAccumulatorData :: FilePath -> AccumulatorData -> IO ()
putUtxoAccumulatorData fp = writeFileJSON fp . toList
