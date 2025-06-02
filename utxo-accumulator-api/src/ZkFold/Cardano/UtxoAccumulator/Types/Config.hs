module ZkFold.Cardano.UtxoAccumulator.Types.Config where

import GeniusYield.Types (GYAddress, GYNetworkId, GYProviders, GYSomePaymentSigningKey, GYSomeStakeSigningKey, GYTxOutRef, GYValue)
import Prelude (FilePath, Maybe, String)

data Config = Config
  { cfgNetworkId :: GYNetworkId
  , cfgProviders :: GYProviders
  , cfgMaestroToken :: String
  , cfgDatabasePath :: FilePath
  , cfgPaymentKey :: GYSomePaymentSigningKey
  , cfgStakeKey :: Maybe GYSomeStakeSigningKey
  , cfgAddress :: GYAddress
  , cfgAccumulationValue :: GYValue
  , cfgMaybeScriptRef :: Maybe GYTxOutRef
  , cfgMaybeThreadTokenRef :: Maybe GYTxOutRef
  }
