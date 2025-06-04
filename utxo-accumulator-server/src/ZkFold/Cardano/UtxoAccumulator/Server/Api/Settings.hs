module ZkFold.Cardano.UtxoAccumulator.Server.Api.Settings (
  SettingsAPI,
  handleSettings,
) where

import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Swagger qualified as Swagger
import Data.Version (showVersion)
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&), (>>>))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import PackageInfo_utxo_accumulator_server qualified as PackageInfo
import Servant
import ZkFold.Cardano.UtxoAccumulator.Server.Orphans ()
import ZkFold.Cardano.UtxoAccumulator.Server.Utils
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))

type SettingsPrefix :: Symbol
type SettingsPrefix = "settings"

data Settings = Settings
  { settingsNetwork :: !String
  , settingsVersion :: !String
  , settingsAccumulationValue :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SettingsPrefix, CamelToSnake]] Settings

instance Swagger.ToSchema Settings where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SettingsPrefix}
      & addSwaggerDescription "UTxO Accumulator server settings."

type SettingsAPI = Summary "Server settings" :> Description "Get server settings such as network and version." :> Get '[JSON] Settings

handleSettings :: Config -> IO Settings
handleSettings cfg@Config {..} = do
  logInfo cfg "Settings API requested."
  pure $
    Settings
      { settingsNetwork = cfgNetworkId & customShowNetworkId
      , settingsVersion = showVersion PackageInfo.version
      , settingsAccumulationValue = show cfgAccumulationValue
      }

-- >>> customShowNetworkId GYMainnet
-- "mainnet"
-- >>> customShowNetworkId GYTestnetLegacy
-- "legacy"
-- >>> customShowNetworkId GYPrivnet
-- "privnet"
customShowNetworkId :: GYNetworkId -> String
customShowNetworkId = show >>> removePrefix "GY" >>> removePrefix "Testnet" >>> lowerFirstChar
 where
  removePrefix :: String -> String -> String
  removePrefix pref str
    | pref `isPrefixOf` str = drop (length pref) str
    | otherwise = str
  lowerFirstChar :: String -> String
  lowerFirstChar "" = ""
  lowerFirstChar (x : xs) = toLower x : xs
