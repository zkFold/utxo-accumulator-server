module ZkFold.Cardano.UtxoAccumulator.Server.Api.Tx (
  TransactionAPI,
  handleTransaction,
) where

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
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TransactionPrefix, CamelToSnake]] Transaction

instance Swagger.ToSchema Transaction where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @TransactionPrefix}
      & addSwaggerDescription "UTxO Accumulator transaction."

type TransactionAPI = Summary "Transaction" :> Description "Build a UTxO Accumulator transaction." :> ReqBody '[JSON] Transaction :> Post '[JSON] GYTx

handleTransaction ::
  Config ->
  Transaction ->
  IO GYTx
handleTransaction cfg Transaction {..} = do
  logInfo cfg "Transaction API requested."
  addUtxoRun cfg (addressFromBech32 txSender) (addressFromBech32 txRecipient) (fromConstant txNonceL) (fromConstant txNonceR) txDistributionTime
