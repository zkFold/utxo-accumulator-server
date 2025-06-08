module ZkFold.Cardano.UtxoAccumulator.Server.RSA (
    RSAKeyPair,
    generateRSAKeyPair,
    getPublicKeyNumbers,
    decryptWithPrivateKey
) where

import           Crypto.PubKey.RSA (PrivateKey, PublicKey, generate, public_e, public_n)
import           Crypto.PubKey.RSA.PKCS15 (decrypt)
import           Data.ByteString (ByteString)

-- | Holds the in-memory RSA key pair
data RSAKeyPair = RSAKeyPair PrivateKey PublicKey

-- | Generate a new RSA key pair (2048 bits)
generateRSAKeyPair :: IO RSAKeyPair
generateRSAKeyPair = do
    (pub, priv) <- generate 2048 65537
    pure $ RSAKeyPair priv pub

-- | Get the public key parameters (n, e) as numbers for sharing with clients
getPublicKeyNumbers :: RSAKeyPair -> (Integer, Integer)
getPublicKeyNumbers (RSAKeyPair _ pub) =
    (public_n pub, public_e pub)

-- | Decrypt a message using the private key
-- Returns either an error or the decrypted bytes
decryptWithPrivateKey :: RSAKeyPair -> ByteString -> Either String ByteString
decryptWithPrivateKey (RSAKeyPair priv _) ciphertext =
    case decrypt Nothing priv ciphertext of
        Left err -> Left (show err)
        Right pt -> Right pt
