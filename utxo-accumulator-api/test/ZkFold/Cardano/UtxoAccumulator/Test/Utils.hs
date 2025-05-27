module ZkFold.Cardano.UtxoAccumulator.Test.Utils where

import GeniusYield.Test.Privnet.Ctx
import GeniusYield.TxBuilder
import GeniusYield.Types

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
