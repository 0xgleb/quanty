module Main (main) where

import Api (app)
import Network.Wai.Handler.Warp (run)
import Protolude


main :: IO ()
main = do
  putStrLn ("Starting Quanty API server on port 8080..." :: Text)
  run 8080 app
