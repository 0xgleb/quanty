module GenerateOpenApi (main) where

import Api (apiOpenApi)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Protolude


main :: IO ()
main = do
  let openApiJson = Aeson.encode apiOpenApi
  LBS.writeFile "openapi.json" openApiJson
  putStrLn ("Generated openapi.json" :: Text)
