module ZkFold.Cardano.UtxoAccumulator.Server.Api.Tx (
  TransactionAPI,
  handleTransaction,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withObject, (.:), (.:?), (.=))
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
import ZkFold.Cardano.UtxoAccumulator.Server.Utils
import ZkFold.Cardano.UtxoAccumulator.TxBuilder (addUtxoRun)
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)

type TransactionPrefix :: Symbol
type TransactionPrefix = "transaction"

data Transaction = Transaction
  { txSender    :: !GYAddressBech32
  , txRecipient :: !GYAddressBech32
  , txNonceL    :: !Natural
  , txNonceR    :: !Natural
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

type TransactionAPI = Summary "Transaction" :> Description "Build a UTxO Accumulator transaction." :> ReqBody '[JSON] Transaction :> Post '[JSON] GYTx

handleTransaction ::
  UtxoAccumulatorCRS ->
  Config ->
  Transaction ->
  IO GYTx
handleTransaction crs cfg Transaction {..} = do
  logInfo cfg "Transaction API requested."
  addUtxoRun crs cfg
    (addressFromBech32 txSender)
    (addressFromBech32 txRecipient)
    (fromConstant txNonceL)
    (fromConstant txNonceR)
    txDistributionTime
