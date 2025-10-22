module Main (main) where

import Api (api, server)
import Network.Wai.Handler.Warp (run)
import Protolude
import Servant (serve)


main :: IO ()
main = do
  putText "Starting Quanty server on http://localhost:8080"
  run 8080 $ serve api server
