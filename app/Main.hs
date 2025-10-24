module Main (main) where

import Api qualified
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors qualified as Cors
import Protolude


main :: IO ()
main = do
  putStrLn ("Starting Quanty API server on port 8080..." :: Text)
  Warp.run 8080 (Cors.simpleCors Api.app)
