module Api (
  API (..),
  HealthResponse (..),
  PlaceholderResponse (..),
  server,
  app,
) where

import Data.Aeson qualified as Aeson
import Data.Time.Clock qualified as Clock
import Network.Wai qualified as Wai
import Protolude
import Servant (type (:-), type (:>))
import Servant qualified
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.Server qualified as Server
import Servant.Server.Generic (AsServer)


data API mode = API
  { health
      :: mode
        :- "health"
          :> Servant.Get '[Servant.JSON] HealthResponse
  , placeholder
      :: mode
        :- "placeholder"
          :> Servant.Get '[Servant.JSON] PlaceholderResponse
  }
  deriving stock (Generic)


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


-- | Server implementation with named handlers
server :: API AsServer
server =
  API {..}
  where
    health :: Servant.Handler HealthResponse
    health =
      pure $
        HealthResponse
          { status = "ok"
          , version = "0.1.0"
          }

    placeholder :: Servant.Handler PlaceholderResponse
    placeholder = do
      now <- liftIO Clock.getCurrentTime
      pure $
        PlaceholderResponse
          { message = "Hello from Quanty API"
          , timestamp = show now
          }


-- | WAI Application
app :: Wai.Application
app =
  Server.serve
    (Servant.Proxy :: Servant.Proxy ("api" :> "v1" :> NamedRoutes API))
    server
