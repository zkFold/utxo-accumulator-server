module ZkFold.Cardano.UtxoAccumulator.Types.Sync where

import GeniusYield.Types (GYAssetClass, GYNetworkId, GYTxOutRef)
import Prelude (Eq, Show, String)

data SyncParams = SyncParams
  { syncgNetworkId :: GYNetworkId
  , syncMaestroToken :: String
  , syncThreadToken :: GYAssetClass
  , syncStateRef :: GYTxOutRef
  , syncCachePath :: String
  }
  deriving (Show, Eq)
