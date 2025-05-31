module ZkFold.Cardano.UtxoAccumulator.Types.Errors where

import Control.Exception (Exception)
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import Network.HTTP.Types (status400, status500)

data UtxoAccumulatorError
  -- | An internal error occurred in the UTXO accumulator.
  = UtxoAccumulatorInternalError
  -- | An error occurred due to invalid input in the UTXO accumulator.
  | UtxoAccumulatorInputError
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError UtxoAccumulatorError where
  toApiError UtxoAccumulatorInternalError =
    GYApiError
      { gaeErrorCode = "UTXO_ACCUMULATOR__INTERNAL_ERROR"
      , gaeHttpStatus = status500
      , gaeMsg = "An internal error occurred in the UTXO accumulator."
      }
  toApiError UtxoAccumulatorInputError =
    GYApiError
      { gaeErrorCode = "UTXO_ACCUMULATOR__INPUT_ERROR"
      , gaeHttpStatus = status400
      , gaeMsg = "An error occurred due to invalid input in the UTXO accumulator."
      }
