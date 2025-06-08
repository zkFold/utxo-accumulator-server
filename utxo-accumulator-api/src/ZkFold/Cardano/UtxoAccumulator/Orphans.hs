{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.UtxoAccumulator.Orphans where

import Data.Aeson (FromJSONKey (..), ToJSONKey)
import GeniusYield.Types (GYTxOutRef)

instance FromJSONKey GYTxOutRef
instance ToJSONKey GYTxOutRef
