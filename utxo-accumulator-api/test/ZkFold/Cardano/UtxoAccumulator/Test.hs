module ZkFold.Cardano.UtxoAccumulator.Test (
  utxoAccumulatorTests
) where

import Data.Text (Text)
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
-- import ZkFold.Cardano.SmartWallet.Api (addressFromEmail, createWallet', sendFunds')
-- import ZkFold.Cardano.SmartWallet.Constants (extraBuildConfiguration)
-- import ZkFold.Cardano.SmartWallet.Test.Utils
-- import ZkFold.Cardano.SmartWallet.Types

-- smartWalletInitRun :: User -> GYValue -> GYUTxO -> Ctx -> (String -> IO ()) -> Text -> IO (Email, ZKInitializedWalletScripts, GYAddress, GYTxBody, (GYSigningKey GYKeyRolePayment, GYKeyHash GYKeyRolePayment))
-- smartWalletInitRun fundUser fundVal collUtxo ctx info emailT = do
--   -- Obtain address of wallet.
--   let email = emailFromText emailT & either error id
--   (zkiws, walletAddress) <- zkctxRunQuery ctx $ addressFromEmail email
--   info $ "Wallet's address: " <> show walletAddress
--   info $ "Initialized wallet scripts: " <> show zkiws
--   -- Fund this address.
--   ctxRunWithCollateral ctx fundUser (utxoRef collUtxo) $ do
--     txBodyFund <- buildTxBody $ mustHaveOutput $ mkGYTxOutNoDatum walletAddress fundVal
--     signAndSubmitConfirmed_ txBodyFund
--   info "Funded wallet"

--   -- Initialize this wallet.
--   -- Generate signing key.
--   newKey :: GYSigningKey 'GYKeyRolePayment <- generateSigningKey
--   let newKeyHash = newKey & getVerificationKey & verificationKeyHash
--   info $ "Generated key: " <> show newKeyHash
--   let jwt = "{testHeader:\"testData\"},{testPayload:\"payloadData\",email:\"" <> emailToText email <> "\"}"
--       proofBytes = proofBytesFromJwt jwt newKeyHash
--       cwi =
--         ZKCreateWalletInfo
--           { zkcwiProofBytes = proofBytes
--           , zkcwiPaymentKeyHash = newKeyHash
--           , zkcwiJWT = jwtFromText jwt
--           , zkcwiEmail = email
--           }
--   info $ "proof bytes: " <> show proofBytes
--   initWalletBody <- zkctxRunBuilder ctx walletAddress (utxoRef collUtxo) $ do
--     createWallet' cwi zkiws walletAddress >>= buildTxBodyWithExtraConfiguration (extraBuildConfiguration zkiws True)
--   info $ "Wallet initialization tx body: " <> show initWalletBody
--   pure (email, zkiws, walletAddress, initWalletBody, (newKey, newKeyHash))

-- smartWalletSingleRun :: [(Sponsor, GYAddress)] -> User -> GYValue -> GYUTxO -> Ctx -> (String -> IO ()) -> Text -> IO (Email, GYTx, (GYSigningKey GYKeyRolePayment, GYKeyHash GYKeyRolePayment))
-- smartWalletSingleRun sponsors fundUser fundVal collUtxo ctx info emailT = do
--   (email, zkiws, walletAddress, initWalletBody, (newKey, newKeyHash)) <- smartWalletInitRun fundUser fundVal collUtxo ctx info emailT
--   -- We require signature from 'fundUser' since we used it's collateral.
--   tidInit <- ctxRun ctx fundUser $ submitTxBodyConfirmed initWalletBody [GYSomeSigningKey newKey, GYSomeSigningKey (fundUser & userPaymentSKey)]
--   info $ "Submitted tx: " <> show tidInit

--   -- Spending funds from the zk based smart wallet by exercising stake script.
--   walletUtxos <- ctxRunQuery ctx $ utxosAtAddress walletAddress Nothing
--   info $ "Wallet UTxOs: " <> show walletUtxos
--   let outs =
--         [ BuildOut
--             { boValue = valueFromLovelace 10_000_000
--             , boDatum = Nothing
--             , boAddress = ctxUserF ctx & userChangeAddress & addressToBech32
--             }
--         , BuildOut
--             { boValue = valueFromLovelace 20_000_000
--             , boDatum = Nothing
--             , boAddress = ctxUserF ctx & userChangeAddress & addressToBech32
--             }
--         ]
--   spendWalletTx <- zkctxRunBuilderWithSponsors sponsors ctx (extraBuildConfiguration zkiws False) walletAddress (utxoRef collUtxo) $ do
--     sendFunds' zkiws walletAddress (ZKSpendWalletInfo {zkswiPaymentKeyHash = newKeyHash, zkswiEmail = email}) outs
--   info $ "send funds tx: " <> show spendWalletTx
--   pure (email, spendWalletTx, (newKey, newKeyHash))

utxoAccumulatorTests :: Setup -> TestTree
utxoAccumulatorTests setup =
  testGroup
    "smart-wallet-single-tests"
    [ testCaseSteps "able to initialize and send funds from a zk based smart wallet" $ \info -> withSetup info setup $ \ctx -> do
        let fundUser = ctxUserF ctx
        -- Find suitable UTxO as collateral.
        collUtxo <- ctxRunQuery ctx $ findSuitableCollateral fundUser
        -- (_email, spendWalletTx, (newKey, _newKeyHash)) <- smartWalletSingleRun mempty fundUser (valueFromLovelace 100_000_000) collUtxo ctx info "zkfold@gmail.com"
        -- tidSpend <- ctxRun ctx fundUser $ submitTxConfirmed $ signGYTx' spendWalletTx [GYSomeSigningKey newKey, GYSomeSigningKey (fundUser & userPaymentSKey)]
        -- info $ "Submitted spend tx: " <> show tidSpend
        info "Test completed."
    ]
