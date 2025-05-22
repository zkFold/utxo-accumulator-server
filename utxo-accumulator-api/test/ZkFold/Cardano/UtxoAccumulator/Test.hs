module ZkFold.Cardano.UtxoAccumulator.Test (
  utxoAccumulatorTests
) where

import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Cardano.UtxoAccumulator.Api (initAccumulator)
import PlutusLedgerApi.V3 (Value)
import Control.Monad.Reader (runReaderT)
import ZkFold.Cardano.UtxoAccumulator.Constants (protocolTreasuryAddress, N, M)
import ZkFold.Symbolic.Examples.UtxoAccumulator (utxoAccumulatorProverSetupInit, utxoAccumulatorGroupElements)

fundingRun :: User -> GYAddress -> GYValue -> Ctx -> IO ()
fundingRun treasury serverAddr serverFunds ctx = ctxRun ctx treasury $ do
    txBodyFund <- buildTxBody $ mustHaveOutput $ mkGYTxOutNoDatum serverAddr serverFunds
    signAndSubmitConfirmed_ txBodyFund

initAccumulatorRun :: GYAddress -> GYPaymentSigningKey -> GYStakeSigningKey -> Value -> Ctx -> IO (GYTxSkeleton 'PlutusV2)
initAccumulatorRun serverAddr serverPaymentKey serverStakeKey v ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey serverPaymentKey)
  (Just $ AGYStakeSigningKey serverStakeKey)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    ttRef <- head <$> utxoRefsAtAddress serverAddr
    txSkel <- initAccumulator v `runReaderT` ttRef
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [serverPaymentKey]
    return txSkel

utxoAccumulatorTests :: Setup -> TestTree
utxoAccumulatorTests setup =
  testGroup
    "utxo-accumulator-tests"
    [ testCaseSteps "able to initialize the server, initialize the accumulator, and perform accumulation-distribution process" $
      \info -> withSetup info setup $ \ctx -> do
        let nid = ctxNetworkId ctx
            treasury = ctxUserF ctx

        -- Generating server keys
        serverPaymentKey :: GYSigningKey 'GYKeyRolePayment <- generateSigningKey
        serverStakeKey   :: GYSigningKey 'GYKeyRoleStaking <- generateSigningKey
        let serverPaymentKeyHash = serverPaymentKey & getVerificationKey & verificationKeyHash
            serverStakeKeyHash = serverStakeKey & getVerificationKey & verificationKeyHash
            serverAddr = addressFromCredential nid (GYPaymentCredentialByKey serverPaymentKeyHash) (Just $ GYStakeCredentialByKey serverStakeKeyHash)

        info "UTXO Accumulator computations ..."
        info $ show $ utxoAccumulatorGroupElements @N @M

        -- Funding the server
        info $ "Server's address: " <> show serverAddr
        fundingRun treasury serverAddr (valueFromLovelace 2500_000_000) ctx
        ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
          >>= info . ("Funded server. Server utxos: " <> ) . show

        txInit <- initAccumulatorRun serverAddr serverPaymentKey serverStakeKey (lovelaceValueOf 100_000_000) ctx
        info $ "Transaction: " <> show txInit
        ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
          >>= info . ("Initialized accumulator. Server's utxos: " <> ) . show

        ctxRunQuery ctx (utxosAtAddress protocolTreasuryAddress Nothing)
          >>= info . ("End-to-end run completed. Treasury's utxos: " <> ) . show
    ]
