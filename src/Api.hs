{-# LANGUAGE DataKinds #-}

module Api
  ( API
  , server
  , api
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Protolude
import Servant


data HealthResponse = HealthResponse
  { status :: Text
  , message :: Text
  }
  deriving (Generic, Show)


instance ToJSON HealthResponse
instance FromJSON HealthResponse


data InfoResponse = InfoResponse
  { name :: Text
  , version :: Text
  , description :: Text
  }
  deriving (Generic, Show)


instance ToJSON InfoResponse
instance FromJSON InfoResponse


type API =
  "health" :> Get '[JSON] HealthResponse
    :<|> "api" :> "info" :> Get '[JSON] InfoResponse


api :: Proxy API
api = Proxy


server :: Server API
server = healthHandler :<|> infoHandler
 where
  healthHandler :: Handler HealthResponse
  healthHandler =
    return $
      HealthResponse
        { status = "ok"
        , message = "Server is running"
        }

  infoHandler :: Handler InfoResponse
  infoHandler =
    return $
      InfoResponse
        { name = "Quanty API"
        , version = "0.1.0"
        , description = "A placeholder API for quantitative finance"
        }
