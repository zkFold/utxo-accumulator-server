module ZkFold.Cardano.UtxoAccumulator.Types.Sync where

import GeniusYield.Types (GYNetworkId, GYTxOutRef, GYAssetClass)
import Prelude (String)

data SyncParams = SyncParams
  { syncgNetworkId :: GYNetworkId
  , syncMaestroToken :: String
  , syncThreadToken :: GYAssetClass
  , syncStateRef :: GYTxOutRef
  }
