module ZkFold.Cardano.UtxoAccumulator.Server.Utils (
  ExceptionTypes (..),
  isMatchedException,
  logInfo,
  logDebug,
  dropSymbolAndCamelToSnake,
  addSwaggerDescription,
  addSwaggerExample,
  bytestringToString,
  parseNatural,
  readHexToNatural,
  writeNaturalToHex,
) where

import Control.Exception (Exception (..), SomeException)
import Data.Aeson (Value (..))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GeniusYield.Imports
import GeniusYield.Swagger.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake)
import GeniusYield.Types (gyLogDebug, gyLogInfo)
import Numeric (showHex)
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

parseNatural :: Value -> Parser Natural
parseNatural (String t) =
  case T.stripPrefix "0x" t of
    Just hex ->
      case readHexToNatural hex of
        Just n -> pure n
        Nothing -> fail $ "Invalid hex string for Natural: " ++ T.unpack t
    Nothing -> fail $ "Expected hex string with 0x prefix for Natural, got: " ++ T.unpack t
parseNatural v = fail $ "Expected hex string for Natural, got: " ++ show v

bytestringToString :: ByteString -> String
bytestringToString = decodeUtf8Lenient >>> T.unpack

logDebug :: HasCallStack => Config -> String -> IO ()
logDebug Config {..} = gyLogDebug cfgProviders mempty

logInfo :: HasCallStack => Config -> String -> IO ()
logInfo Config {..} = gyLogInfo cfgProviders mempty

readHexToNatural :: T.Text -> Maybe Natural
readHexToNatural t = case TR.hexadecimal t of
  Right (i, rest) | T.null rest && i >= 0 -> Just (fromIntegral (i :: Integer))
  _ -> Nothing

-- | Convert a Natural to a 0x-prefixed, 56-digit hex string (for BLS12-381 scalar nonces)
writeNaturalToHex :: Natural -> T.Text
writeNaturalToHex n =
  let hex = T.pack (showHex n "")
      padded = T.justifyRight 56 '0' hex
   in T.append "0x" padded
