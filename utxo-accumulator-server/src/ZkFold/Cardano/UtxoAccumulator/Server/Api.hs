module ZkFold.Cardano.UtxoAccumulator.Server.Api (
  UtxoAccumulatorAPI,
  utxoAccumulatorAPI,
  utxoAccumulatorServer,
  MainAPI,
  mainAPI,
  mainServer,
  utxoAccumulatorAPIOpenApi,
) where

import Control.Lens ((.~), (?~))
import Data.Kind (Type)
import Data.OpenApi
import Data.OpenApi qualified as OpenApi
import GeniusYield.Imports ((&))
import GeniusYield.Types (GYTxOutRef)
import GeniusYield.Types.OpenApi ()
import Servant
import Servant.OpenApi
import ZkFold.Cardano.UtxoAccumulator.Server.Api.Settings (SettingsAPI, handleSettings)
import ZkFold.Cardano.UtxoAccumulator.Server.Api.Tx (TransactionAPI, handleTransaction)
import ZkFold.Cardano.UtxoAccumulator.Server.Auth (APIKeyAuthProtect, V0)
import ZkFold.Cardano.UtxoAccumulator.Server.Orphans ()
import ZkFold.Cardano.UtxoAccumulator.Server.RSA (RSAKeyPair)
import ZkFold.Cardano.UtxoAccumulator.Types (Config)
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)

-------------------------------------------------------------------------------
-- Server's API.
-------------------------------------------------------------------------------

type V0API =
  "settings" :> SettingsAPI
    :<|> "transaction" :> TransactionAPI

type UtxoAccumulatorAPI = APIKeyAuthProtect :> V0 :> V0API

utxoAccumulatorAPI :: Proxy UtxoAccumulatorAPI
utxoAccumulatorAPI = Proxy

infixr 4 +>

type family (+>) (api1 :: k) (api2 :: Type) where
  (+>) api1 api2 = APIKeyAuthProtect :> V0 :> api1 :> api2

utxoAccumulatorAPIOpenApi :: OpenApi
utxoAccumulatorAPIOpenApi =
  toOpenApi utxoAccumulatorAPI
    & info
    . OpenApi.title
    .~ "UTxO Accumulator server API"
      & info
      . version
    .~ "0.0.1"
      & info
      . license
    ?~ ("Apache-2.0" & url ?~ URL "https://opensource.org/licenses/apache-2-0")
      & info
      . OpenApi.description
    ?~ "API to interact with UTxO Accumulator."
      & applyTagsFor (subOperations (Proxy :: Proxy ("settings" +> SettingsAPI)) (Proxy :: Proxy UtxoAccumulatorAPI)) ["Settings" & OpenApi.description ?~ "Endpoint to get server settings such as network and version"]
      & applyTagsFor (subOperations (Proxy :: Proxy ("transaction" +> TransactionAPI)) (Proxy :: Proxy UtxoAccumulatorAPI)) ["Transaction" & OpenApi.description ?~ "Endpoint to make a transaction using the UTxO Accumulator"]

utxoAccumulatorServer :: RSAKeyPair -> UtxoAccumulatorCRS -> Config -> GYTxOutRef -> ServerT UtxoAccumulatorAPI IO
utxoAccumulatorServer rsaKeyPair crs cfg ref =
  ignoredAuthResult $
    handleSettings rsaKeyPair cfg ref
      :<|> handleTransaction crs cfg ref rsaKeyPair
 where
  ignoredAuthResult f _authResult = f

type MainAPI =
  UtxoAccumulatorAPI

mainAPI :: Proxy MainAPI
mainAPI = Proxy

mainServer :: RSAKeyPair -> UtxoAccumulatorCRS -> Config -> GYTxOutRef -> ServerT MainAPI IO
mainServer = utxoAccumulatorServer
