module ZkFold.Cardano.UtxoAccumulator.Test.Utils where

import Control.Monad.Reader (ReaderT (..))
import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString, unpack)
import Data.Default (Default (..))
import Data.Foldable (find)
import Data.Text.Encoding (encodeUtf8)
import GeniusYield.Imports (Text, (&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Transaction.Common (GYTxExtraConfiguration)
import GeniusYield.TxBuilder
import GeniusYield.Types
import System.Random (mkStdGen)
import ZkFold.Algebra.Class (zero)
-- import ZkFold.Cardano.SmartWallet.Api.Sponsor (applySponsor)
-- import ZkFold.Cardano.SmartWallet.Constants (zkWalletBuildInfo)
-- import ZkFold.Cardano.SmartWallet.Types
-- import ZkFold.Cardano.SmartWallet.Utils (valueAtleastLovelace, valueIsAdaOnly)
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))

-- zkctxRunQuery :: Ctx -> ReaderT ZKWalletBuildInfo GYTxQueryMonadIO a -> IO a
-- zkctxRunQuery ctx q = runGYTxQueryMonadIO (ctxNetworkId ctx) (ctxProviders ctx) $ runReaderT q zkWalletBuildInfo

-- zkctxRunBuilder :: Ctx -> GYAddress -> GYTxOutRef -> ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO a -> IO a
-- zkctxRunBuilder ctx walletAddr coll b = runGYTxBuilderMonadIO (ctxNetworkId ctx) (ctxProviders ctx) [walletAddr] walletAddr (Just (coll, False)) $ runReaderT b zkWalletBuildInfo

-- zkctxRunBuilderWithSponsors :: forall v. [(Sponsor, GYAddress)] -> Ctx -> GYTxExtraConfiguration v -> GYAddress -> GYTxOutRef -> ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO (GYTxSkeleton v) -> IO GYTx
-- zkctxRunBuilderWithSponsors sponsors ctx ec walletAddr coll b =
--   runGYTxBuilderMonadIO (ctxNetworkId ctx) (ctxProviders ctx) [walletAddr] walletAddr (Just (coll, False)) $ do
--     skel <- runReaderT b zkWalletBuildInfo
--     utxos <- utxosAtAddress walletAddr Nothing
--     applySponsor sponsors utxos skel walletAddr def ec

-- ctxRunWithCollateral :: Ctx -> User -> GYTxOutRef -> GYTxMonadIO a -> IO a
-- ctxRunWithCollateral ctx User' {..} coll =
--   runGYTxMonadIO
--     (ctxNetworkId ctx)
--     (ctxProviders ctx)
--     (AGYPaymentSigningKey userPaymentSKey')
--     (AGYStakeSigningKey <$> userStakeSKey')
--     [userAddr]
--     userAddr
--     (Just (coll, False))

-- proofBytesFromJwt :: forall kr. Text -> GYKeyHash kr -> ZKProofBytes
-- proofBytesFromJwt jwt (keyHashToRawBytes -> keyHash) = zkpBytes
--  where
--   -- Generate an RSA key pair. Private key will be used to sign the JWT (in real life that will be done by Google),
--   -- and the public key will be sent to the wallet script alongside the signature.
--   --
--   (R.PublicKey {..}, R.PrivateKey {..}, _) = generateKeyPair (mkStdGen 42) 2048

--   h = hash (encodeUtf8 jwt)

--   bsToNat :: ByteString -> Natural
--   bsToNat = foldl (\a w -> fromIntegral w + 256 * a) 0 . unpack

--   hNat :: Natural
--   hNat = bsToNat h

--   prD, e, n :: Natural
--   prD = fromIntegral private_d
--   e = fromIntegral public_e
--   n = fromIntegral public_n

--   expM :: Natural -> Natural -> Natural -> Natural
--   expM _ 0 _ = 1
--   expM b ex m =
--     case ex `mod` 2 of
--       1 -> (b * expM b (ex - 1) m) `mod` m
--       _ ->
--         let e2 = expM b (ex `div` 2) m
--          in (e2 * e2) `mod` m

--   msg = expM hNat prD n

--   keyNat :: Natural
--   keyNat = bsToNat keyHash

--   zkpBytes :: ZKProofBytes
--   zkpBytes = mkProof $ expModProofMock @ByteString zero (PlonkupProverSecret $ pure zero) (ExpModProofInput e n msg keyNat)

-- -- | Find a suitable collateral UTxO.
-- findSuitableCollateral :: GYTxQueryMonad m => User -> m GYUTxO
-- findSuitableCollateral user = do
--   let userAddr = userChangeAddress user
--   utxos <- utxosAtAddress userAddr Nothing
--   case utxosToList utxos & reverse & find (\GYUTxO {..} -> valueIsAdaOnly utxoValue && valueAtleast5Ada utxoValue) of
--     Nothing -> throwError $ GYNoSuitableCollateralException 5_000_000 userAddr
--     Just utxo -> pure utxo
--  where
--   valueAtleast5Ada :: GYValue -> Bool
--   valueAtleast5Ada val = valueAtleastLovelace val 5_000_000
