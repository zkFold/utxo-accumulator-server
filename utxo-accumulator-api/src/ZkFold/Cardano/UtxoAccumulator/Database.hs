module ZkFold.Cardano.UtxoAccumulator.Database where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Functor ((<&>))
import Data.Map (Map, fromList, toList)
import Data.String (fromString)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import GeniusYield.Types (GYAddress, GYTxOutRef, addressFromBech32, addressToBech32)
import System.Directory (doesFileExist, removeFile)
import ZkFold.Algebra.Class (fromConstant)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (fromZp)
import ZkFold.Cardano.UtxoAccumulator.Utils (parseNatural, writeNaturalToHex)
import ZkFold.Prelude (readFileJSON, writeFileJSON)

data AccumulatorDataKey = AccumulatorDataKey
  { adkAddress :: GYAddress
  , adkNonceL :: ScalarFieldOf BLS12_381_G1_Point
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AccumulatorDataKey where
  parseJSON = withObject "AccumulatorDataKey" $ \o -> do
    addrStr <- o .: "adkAddress"
    let addr = addressFromBech32 (fromString addrStr)
    nonceNat <- o .: "adkNonceL" >>= parseNatural
    let nonce = fromConstant nonceNat
    pure $ AccumulatorDataKey addr nonce

instance ToJSON AccumulatorDataKey where
  toJSON (AccumulatorDataKey addr nonceL) =
    object
      [ "adkAddress" .= addressToBech32 addr
      , "adkNonceL" .= writeNaturalToHex (fromZp nonceL)
      ]

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
  , adiThreadTokenRef :: GYTxOutRef
  }
  deriving (Show, Eq, Generic)

instance ToJSON AccumulatorDataItem where
  toJSON (AccumulatorDataItem nonceR distTime threadRef) =
    object
      [ "adiNonceR" .= writeNaturalToHex (fromZp nonceR)
      , "adiDistributionTime" .= distTime
      , "adiThreadTokenRef" .= threadRef
      ]

instance FromJSON AccumulatorDataItem where
  parseJSON = withObject "AccumulatorDataItem" $ \o ->
    AccumulatorDataItem
      <$> ((o .: "adiNonceR" >>= parseNatural) <&> fromConstant)
      <*> o .: "adiDistributionTime"
      <*> o .: "adiThreadTokenRef"

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
