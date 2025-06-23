module ZkFold.Cardano.UtxoAccumulator.Server.Api.Tx (
  TransactionAPI,
  handleTransaction,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), eitherDecodeStrict, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS
import Data.Swagger qualified as Swagger
import Data.Time.Clock.POSIX (POSIXTime)
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import Servant
import ZkFold.Algebra.Class (fromConstant)
import ZkFold.Cardano.UtxoAccumulator.Server.Orphans ()
import ZkFold.Cardano.UtxoAccumulator.Server.RSA (RSAKeyPair, decryptWithPrivateKey)
import ZkFold.Cardano.UtxoAccumulator.Server.Utils
import ZkFold.Cardano.UtxoAccumulator.TxBuilder (addUtxoRun)
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)

type TransactionPrefix :: Symbol
type TransactionPrefix = "transaction"

data Transaction = Transaction
  { txSender :: !GYAddressBech32
  , txRecipient :: !GYAddressBech32
  , txNonceL :: !Natural
  , txNonceR :: !Natural
  , txDistributionTime :: !(Maybe POSIXTime)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o ->
    Transaction
      <$> o .: "tx_sender"
      <*> o .: "tx_recipient"
      <*> (o .: "tx_nonce_l" >>= parseNatural)
      <*> (o .: "tx_nonce_r" >>= parseNatural)
      <*> o .:? "tx_distribution_time"

instance Swagger.ToSchema Transaction where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @TransactionPrefix}
      & addSwaggerDescription "UTxO Accumulator transaction."

instance ToJSON Transaction where
  toJSON Transaction {..} =
    object
      [ "tx_sender" .= txSender
      , "tx_recipient" .= txRecipient
      , "tx_nonce_l" .= String (writeNaturalToHex txNonceL)
      , "tx_nonce_r" .= String (writeNaturalToHex txNonceR)
      , "tx_distribution_time" .= txDistributionTime
      ]

newtype EncryptedTransaction = EncryptedTransaction {unEncryptedTransaction :: String}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Swagger.ToSchema EncryptedTransaction where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Base64-encoded RSA-encrypted Transaction payload."

type TransactionAPI = Summary "Transaction" :> Description "Build a UTxO Accumulator transaction (encrypted)." :> ReqBody '[JSON] EncryptedTransaction :> Post '[JSON] GYTx

handleTransaction ::
  UtxoAccumulatorCRS ->
  Config ->
  GYTxOutRef ->
  RSAKeyPair ->
  EncryptedTransaction ->
  IO GYTx
handleTransaction crs cfg ref rsaKeyPair (EncryptedTransaction b64) = do
  logInfo cfg "Transaction API requested (encrypted)."
  case B64.decode (BS.pack b64) >>= decryptWithPrivateKey rsaKeyPair >>= eitherDecodeStrict of
    Left err -> fail $ "Transaction decode failed: " ++ err
    Right Transaction {..} ->
      addUtxoRun
        crs
        cfg
        ref
        (addressFromBech32 txSender)
        (addressFromBech32 txRecipient)
        (fromConstant txNonceL)
        (fromConstant txNonceR)
        txDistributionTime
