module ZkFold.Cardano.UtxoAccumulator.Test.Utils where

import GeniusYield.Test.Privnet.Ctx
import GeniusYield.TxBuilder
import GeniusYield.Types

-- zkctxRunBuilder :: Ctx -> GYAddress -> GYTxOutRef -> ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO a -> IO a
-- zkctxRunBuilder ctx walletAddr coll b = runGYTxBuilderMonadIO (ctxNetworkId ctx) (ctxProviders ctx) [walletAddr] walletAddr (Just (coll, False)) $ runReaderT b zkWalletBuildInfo

ctxRun :: Ctx -> User -> GYTxMonadIO a -> IO a
ctxRun ctx User' {..} =
  runGYTxMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    (AGYPaymentSigningKey userPaymentSKey')
    (AGYStakeSigningKey <$> userStakeSKey')
    [userAddr]
    userAddr
    Nothing

ctxRunWithCollateral :: Ctx -> User -> GYTxOutRef -> GYTxMonadIO a -> IO a
ctxRunWithCollateral ctx User' {..} coll =
  runGYTxMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    (AGYPaymentSigningKey userPaymentSKey')
    (AGYStakeSigningKey <$> userStakeSKey')
    [userAddr]
    userAddr
    (Just (coll, False))
