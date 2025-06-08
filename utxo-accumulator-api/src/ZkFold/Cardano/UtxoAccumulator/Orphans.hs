{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.UtxoAccumulator.Orphans where

import Data.Aeson (FromJSONKey (..))
import GeniusYield.Types (GYTxOutRef)

instance FromJSONKey GYTxOutRef
