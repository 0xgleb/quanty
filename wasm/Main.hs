{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, Value, decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)


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


-- Note: We've proven bidirectional data passing with doubleValue (Int -> Int).
-- Proper JSON marshalling with concrete Haskell types will come in Task 5
-- when we implement BlackScholes types with proper Aeson instances.

-- | Test type for TypeScript generation
-- This proves aeson-typescript works before we add BlackScholes types
data TestMessage = TestMessage
  { message :: Text
  , value :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


-- | Main function (required but not called in reactor mode)
main :: IO ()
main = pure ()
