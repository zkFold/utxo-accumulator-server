module ZkFold.Cardano.UtxoAccumulator.Server.Api.Tx (
  TransactionAPI,
  handleTransaction,
) where

import Data.Swagger qualified as Swagger
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import Servant
import ZkFold.Cardano.UtxoAccumulator.Server.Orphans ()
import ZkFold.Cardano.UtxoAccumulator.Server.Utils
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))
import ZkFold.Cardano.UtxoAccumulator.TxBuilder (addUtxoRun)
import ZkFold.Algebra.Class (fromConstant)

type TransactionPrefix :: Symbol
type TransactionPrefix = "transaction"

data Transaction = Transaction
  { txSender    :: !GYAddress
  , txRecipient :: !GYAddress
  , txNonce     :: !Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TransactionPrefix, CamelToSnake]] Transaction

instance Swagger.ToSchema Transaction where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @TransactionPrefix}
      & addSwaggerDescription "UTxO Accumulator transaction."

type TransactionAPI = Summary "Transaction" :> Description "Build a UTxO Accumulator transaction." :> ReqBody '[JSON] Transaction :> Get '[JSON] GYTx

handleTransaction ::
  Config ->
  Transaction ->
  IO GYTx
handleTransaction cfg Transaction {..} = do
  logInfo cfg "Transaction API requested."
  addUtxoRun cfg txSender txRecipient $ fromConstant txNonce
