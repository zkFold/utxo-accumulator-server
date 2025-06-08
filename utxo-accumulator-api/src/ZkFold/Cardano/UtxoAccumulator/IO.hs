module ZkFold.Cardano.UtxoAccumulator.IO where

import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config (..))

runQueryWithConfig :: Config -> GYTxQueryMonadIO a -> IO a
runQueryWithConfig cfg =
  runGYTxQueryMonadIO
    (cfgNetworkId cfg)
    (cfgProviders cfg)

runBuilderWithConfig :: Config -> GYAddress -> GYTxBuilderMonadIO a -> IO a
runBuilderWithConfig cfg addr =
  runGYTxBuilderMonadIO
    (cfgNetworkId cfg)
    (cfgProviders cfg)
    [addr]
    addr
    Nothing

runSignerWithConfig :: Config -> GYTxMonadIO a -> IO a
runSignerWithConfig cfg =
  runGYTxMonadIO
    (cfgNetworkId cfg)
    (cfgProviders cfg)
    (cfgPaymentKey cfg)
    (cfgStakeKey cfg)
    [cfgAddress cfg]
    (cfgAddress cfg)
    Nothing
