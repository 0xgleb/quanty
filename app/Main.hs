{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..),
    cors,
    simpleCorsResourcePolicy,
  )
import Protolude
import Servant
  ( Get,
    Handler,
    JSON,
    Proxy (..),
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )

-- | API response type
data ApiResponse = ApiResponse
  { message :: Text,
    timestamp :: Text,
    status :: Text
  }
  deriving (Generic, Show)

instance ToJSON ApiResponse
instance FromJSON ApiResponse

-- | Health check response
data HealthResponse = HealthResponse
  { healthy :: Bool,
    service :: Text
  }
  deriving (Generic, Show)

instance ToJSON HealthResponse
instance FromJSON HealthResponse

-- | API definition
type API =
  "api" :> "hello" :> Get '[JSON] ApiResponse
    :<|> "api" :> "health" :> Get '[JSON] HealthResponse

-- | Server implementation
server :: Server API
server = helloHandler :<|> healthHandler
  where
    helloHandler :: Handler ApiResponse
    helloHandler = do
      pure $
        ApiResponse
          { message = "Hello from Quanty Servant API!",
            timestamp = "2025-10-22",
            status = "success"
          }

    healthHandler :: Handler HealthResponse
    healthHandler = do
      pure $
        HealthResponse
          { healthy = True,
            service = "quanty-api"
          }

-- | API proxy
api :: Proxy API
api = Proxy

-- | CORS configuration
corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy
    { corsRequestHeaders = ["Content-Type"],
      corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    }

-- | WAI Application with CORS
app :: Application
app = cors (const $ Just corsPolicy) $ serve api server

-- | Main entry point
main :: IO ()
main = do
  putStrLn ("Starting Quanty API server on port 8080..." :: Text)
  run 8080 app
