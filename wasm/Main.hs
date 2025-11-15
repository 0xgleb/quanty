{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import BlackScholes qualified as BS
import Data.Aeson (FromJSON, ToJSON, Value, decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Wasm.Prim (JSString)
import GHC.Wasm.Prim qualified as Wasm


-- | Pure addition function - no IO, should have no scheduler issues
foreign export javascript "addNumbers" addNumbers :: Int -> Int -> Int


addNumbers :: Int -> Int -> Int
addNumbers x y = x + y


-- | Import a JavaScript function that logs a hardcoded message
foreign import javascript "console.log('Hello from Haskell WASM!')"
  js_log :: IO ()


-- | Simple "Hello World" function using console.log instead of putStrLn
-- This avoids WASI stdout buffering issues that cause scheduler loops
foreign export javascript "helloWasm" helloWasm :: IO ()


helloWasm :: IO ()
helloWasm = js_log


-- | Double a number - demonstrates data passing from JS -> Haskell -> JS
foreign export javascript "doubleValue" doubleValue :: Int -> Int


doubleValue :: Int -> Int
doubleValue n = n * 2


-- | Internal helper: validate JSON with Aeson
-- This proves aeson compiles and works in WASM
-- We'll add proper typed JSON functions in Task 5 with BlackScholes types
validateJson :: Text -> Bool
validateJson jsonStr =
  let jsonBytes = BL.fromStrict $ TE.encodeUtf8 jsonStr
   in case decode jsonBytes :: Maybe Value of
        Nothing -> False
        Just _ -> True


-- | Calculate Black-Scholes option price - FFI export for JavaScript
foreign export javascript "calculateBlackScholes"
  calculateBlackScholesFFI :: JSString -> IO JSString


calculateBlackScholesFFI :: JSString -> IO JSString
calculateBlackScholesFFI jsStr = do
  let str = Wasm.fromJSString jsStr -- JSString -> String
  let textStr = T.pack str -- String -> Text
  let jsonBytes = BL.fromStrict $ TE.encodeUtf8 textStr
  case decode jsonBytes of
    Nothing -> pure $ Wasm.toJSString "{\"error\": \"Invalid JSON input\"}"
    Just input -> do
      let result = BS.calculatePriceWithGreeks input
      let resultJson = TE.decodeUtf8 $ BL.toStrict $ encode result
      let resultStr = T.unpack resultJson -- Text -> String
      pure $ Wasm.toJSString resultStr -- String -> JSString


-- | Main function (required but not called in reactor mode)
main :: IO ()
main = pure ()
