module ZkFold.Cardano.UtxoAccumulator.Types.Config where

import Prelude (Maybe, FilePath)
import GeniusYield.Types (GYNetworkId, GYProviders, GYPaymentSigningKey, GYStakeSigningKey, GYAddress, GYTxOutRef, GYValue)

data Config = Config
  { cfgNetworkId           :: GYNetworkId
  , cfgProviders           :: GYProviders
  , cfgPaymentKey          :: GYPaymentSigningKey
  , cfgStakeKey            :: Maybe GYStakeSigningKey
  , cfgAddress             :: GYAddress
  , cfgDatabase            :: FilePath
  , cfgAccumulationValue   :: GYValue
  , cfgMaybeScriptRef      :: Maybe GYTxOutRef
  , cfgMaybeThreadTokenRef :: Maybe GYTxOutRef
  }
