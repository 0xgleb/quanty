{-# LANGUAGE ForeignFunctionInterface #-}

module Main where


-- | Pure addition function - no IO, should have no scheduler issues
foreign export javascript "addNumbers" addNumbers :: Int -> Int -> Int


addNumbers :: Int -> Int -> Int
addNumbers x y = x + y


-- | Import a JavaScript function that logs a hardcoded message
-- We can't easily pass Haskell String to JS, so use a simpler approach
foreign import javascript "console.log('Hello from Haskell WASM!')"
  js_log :: IO ()


-- | Simple "Hello World" function using console.log instead of putStrLn
-- This avoids WASI stdout buffering issues that cause scheduler loops
foreign export javascript "helloWasm" helloWasm :: IO ()


helloWasm :: IO ()
helloWasm = js_log


-- | Main function (required but not called in reactor mode)
main :: IO ()
main = pure ()
