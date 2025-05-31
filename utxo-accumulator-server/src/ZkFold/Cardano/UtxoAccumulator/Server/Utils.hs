module ZkFold.Cardano.UtxoAccumulator.Server.Utils (
  ExceptionTypes (..),
  isMatchedException,
  logInfo,
  logDebug,
  dropSymbolAndCamelToSnake,
  addSwaggerDescription,
  addSwaggerExample,
  bytestringToString,
) where

import Control.Exception (Exception (..), SomeException)
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import GeniusYield.Imports
import GeniusYield.Swagger.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake)
import GeniusYield.Types (gyLogDebug, gyLogInfo)
import ZkFold.Cardano.UtxoAccumulator.Types (Config (..))

type ExceptionTypes :: [Type] -> Type
data ExceptionTypes es where
  ENil :: ExceptionTypes '[]
  (:>>) :: Exception e => Proxy e -> ExceptionTypes es -> ExceptionTypes (e ': es)

infixr 5 :>>

isMatchedException :: ExceptionTypes es -> SomeException -> Bool
isMatchedException ENil _ = False
isMatchedException (etype :>> etypes) se = isJust (f etype) || isMatchedException etypes se
 where
  f :: forall e. Exception e => Proxy e -> Maybe e
  f _ = fromException @e se

bytestringToString :: ByteString -> String
bytestringToString = decodeUtf8Lenient >>> Text.unpack

logDebug :: HasCallStack => Config -> String -> IO ()
logDebug Config {..} = gyLogDebug cfgProviders mempty

logInfo :: HasCallStack => Config -> String -> IO ()
logInfo Config {..} = gyLogInfo cfgProviders mempty
