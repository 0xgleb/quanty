module Api (
  API,
  HealthResponse (..),
  PlaceholderResponse (..),
  server,
  app,
) where

import Data.Aeson qualified as Aeson
import Data.Time.Clock qualified as Clock
import Network.Wai qualified as Wai
import Protolude
import Servant (type (:<|>) (..), type (:>))
import Servant qualified
import Servant.Server qualified as Server


-- | Health check endpoint
type HealthAPI = "health" :> Servant.Get '[Servant.JSON] HealthResponse


-- | Placeholder endpoint
type PlaceholderAPI =
  "placeholder" :> Servant.Get '[Servant.JSON] PlaceholderResponse


-- | Combined API type definition
type API = "api" :> "v1" :> (HealthAPI :<|> PlaceholderAPI)


-- | Health check response
data HealthResponse = HealthResponse
  { status :: Text
  , version :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


-- | Placeholder endpoint response
data PlaceholderResponse = PlaceholderResponse
  { message :: Text
  , timestamp :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


-- | Server implementation
server :: Servant.Server API
server = healthHandler :<|> placeholderHandler
  where
    healthHandler :: Servant.Handler HealthResponse
    healthHandler =
      pure $
        HealthResponse
          { status = "ok"
          , version = "0.1.0"
          }

    placeholderHandler :: Servant.Handler PlaceholderResponse
    placeholderHandler = do
      now <- liftIO Clock.getCurrentTime
      pure $
        PlaceholderResponse
          { message = "Hello from Quanty API"
          , timestamp = show now
          }


-- | WAI Application
app :: Wai.Application
app = Server.serve (Servant.Proxy :: Servant.Proxy API) server
