module ZkFold.Cardano.UtxoAccumulator.Test (
  utxoAccumulatorTests,
) where

import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder
import ZkFold.Cardano.UtxoAccumulator.Constants (protocolTreasuryAddress, utxoAccumulatorAddress, utxoAccumulatorScript)
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config(..))

fundingRun :: User -> GYAddress -> GYValue -> Ctx -> IO ()
fundingRun treasury serverAddr serverFunds ctx = ctxRun ctx treasury $ do
  txBodyFund <-
    buildTxBody $
      mustHaveOutput (mkGYTxOutNoDatum serverAddr serverFunds)
  signAndSubmitConfirmed_ txBodyFund

utxoAccumulatorTests :: Setup -> TestTree
utxoAccumulatorTests setup =
  testGroup
    "utxo-accumulator-tests"
    [ testCaseSteps "able to initialize the server, initialize the accumulator, and perform accumulation-distribution process" $
        \info -> withSetup info setup $ \ctx -> do
          let nid = ctxNetworkId ctx
              user1 = ctxUserF ctx
              user2 = ctxUser2 ctx

          -- Generating server keys
          serverPaymentKey <- generateSigningKey
          serverStakeKey <-  generateSigningKey
          let serverPaymentKeyHash = serverPaymentKey & getVerificationKey & verificationKeyHash
              serverStakeKeyHash = serverStakeKey & getVerificationKey & verificationKeyHash
              serverAddr = addressFromCredential nid (GYPaymentCredentialByKey serverPaymentKeyHash) (Just $ GYStakeCredentialByKey serverStakeKeyHash)

          let cfg = Config
                { cfgNetworkId = nid
                , cfgProviders = ctxProviders ctx
                , cfgPaymentKey = AGYPaymentSigningKey serverPaymentKey
                , cfgStakeKey = Just $ AGYStakeSigningKey serverStakeKey
                , cfgAddress = serverAddr
                , cfgDatabasePath = "txs.json"
                , cfgAccumulationValue = valueFromLovelace 100_000_000
                , cfgMaybeScriptRef = Nothing
                , cfgMaybeThreadTokenRef = Nothing
                }

          -- writeFileJSON "utxo-accumulator-api/accumulation-group-elements.json" (accumulationGroupElements @N @M)
          -- writeFileJSON "utxo-accumulator-api/distribution-group-elements.json" (distributionGroupElements @N @M)

          info $ "Protocol script size: " <> show (scriptSize $ GYPlutusScript $ utxoAccumulatorScript (cfgAccumulationValue cfg))
          ctxRunQuery ctx (utxoAccumulatorAddress (cfgAccumulationValue cfg))
            >>= info . ("Protocol script address: " <>) . show

          -- Funding the server
          info $ "Server's address: " <> show serverAddr
          fundingRun user1 serverAddr (valueFromLovelace 2500_000_000) ctx
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Funded server. Server utxos: " <>) . show

          -- Initializing the script
          cfg' <- postScriptRun cfg
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Posted accumulator script. Server's utxos: " <>) . show

          -- Initializing the accumulator
          cfg'' <- initAccumulatorRun cfg'
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Initialized accumulator. Server's utxos: " <>) . show

          -- Adding funds to the accumulator
          let sender    = userAddr user1
              recipient = userAddr user2
              r = toZp 42
          tx <- addUtxoRun cfg'' sender recipient r

          -- User signs and submits the transaction
          runSignerWithConfig cfg
            { cfgPaymentKey = AGYPaymentSigningKey $ userPaymentSKey user1
            , cfgStakeKey = AGYStakeSigningKey <$> userStakeSKey user1
            , cfgAddress = userAddr user1} $
            submitTxConfirmed_ $ signGYTx tx [userPaymentSKey user1]
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Added a utxo to accumulator. Server's utxos: " <>) . show

          -- Removing funds from the accumulator
          removeUtxoRun cfg''
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Removed a utxo from accumulator. Server's utxos: " <>) . show

          ctxRunQuery ctx (utxosAtAddress protocolTreasuryAddress Nothing)
            >>= info . ("End-to-end run completed. Treasury's utxos: " <>) . show
    ]
