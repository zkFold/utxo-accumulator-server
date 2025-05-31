module ZkFold.Cardano.UtxoAccumulator.Types.Config where

import Prelude (Maybe, FilePath)
import GeniusYield.Types (GYNetworkId, GYProviders, GYAddress, GYTxOutRef, GYValue, GYSomePaymentSigningKey, GYSomeStakeSigningKey)

data Config = Config
  { cfgNetworkId           :: GYNetworkId
  , cfgProviders           :: GYProviders
  , cfgDatabasePath        :: FilePath
  , cfgPaymentKey          :: GYSomePaymentSigningKey
  , cfgStakeKey            :: Maybe GYSomeStakeSigningKey
  , cfgAddress             :: GYAddress
  , cfgAccumulationValue   :: GYValue
  , cfgMaybeScriptRef      :: Maybe GYTxOutRef
  , cfgMaybeThreadTokenRef :: Maybe GYTxOutRef
  }
