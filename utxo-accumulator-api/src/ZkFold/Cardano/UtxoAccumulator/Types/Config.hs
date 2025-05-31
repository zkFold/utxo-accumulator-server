module ZkFold.Cardano.UtxoAccumulator.Types.Config where

import GeniusYield.Types (GYAddress, GYNetworkId, GYProviders, GYSomePaymentSigningKey, GYSomeStakeSigningKey, GYTxOutRef, GYValue)
import Prelude (FilePath, Maybe)

data Config = Config
  { cfgNetworkId :: GYNetworkId
  , cfgProviders :: GYProviders
  , cfgDatabasePath :: FilePath
  , cfgPaymentKey :: GYSomePaymentSigningKey
  , cfgStakeKey :: Maybe GYSomeStakeSigningKey
  , cfgAddress :: GYAddress
  , cfgAccumulationValue :: GYValue
  , cfgMaybeScriptRef :: Maybe GYTxOutRef
  , cfgMaybeThreadTokenRef :: Maybe GYTxOutRef
  }
