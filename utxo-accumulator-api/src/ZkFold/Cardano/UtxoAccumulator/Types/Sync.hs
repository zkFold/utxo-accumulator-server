module ZkFold.Cardano.UtxoAccumulator.Types.Sync where

import GeniusYield.Types (GYAssetClass, GYNetworkId, GYTxOutRef)
import Prelude (String, Show, Eq)

data SyncParams = SyncParams
  { syncgNetworkId :: GYNetworkId
  , syncMaestroToken :: String
  , syncThreadToken :: GYAssetClass
  , syncStateRef :: GYTxOutRef
  }
  deriving (Show, Eq)
