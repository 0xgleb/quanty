module Api (
  API (..),
  HealthResponse (..),
  PlaceholderResponse (..),
  server,
  app,
  apiOpenApi,
) where

import BlackScholes qualified
import Control.Lens ((.~), (?~))
import Data.Aeson qualified as Aeson
import Data.OpenApi (ToSchema)
import Data.OpenApi qualified as OpenApi
import Data.Time.Clock qualified as Clock
import Network.Wai qualified as Wai
import Option (getTimeToExpiryDays)
import Protolude
import Servant (type (:-), type (:>))
import Servant qualified
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.OpenApi qualified
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
  , blackScholes
      :: mode
        :- "black-scholes"
          :> Servant.ReqBody '[Servant.JSON] BlackScholes.Inputs
          :> Servant.Post '[Servant.JSON] BlackScholes.OptionPrice
  }
  deriving stock (Generic)


data HealthResponse = HealthResponse
  { status :: Text
  , version :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


data PlaceholderResponse = PlaceholderResponse
  { message :: Text
  , timestamp :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


server :: API AsServer
server = API {..}
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

    blackScholes :: BlackScholes.Inputs -> Servant.Handler BlackScholes.OptionPrice
    blackScholes input = do
      -- Validate inputs
      when (input.spot <= 0) $
        Servant.throwError
          Servant.err400
            { Servant.errBody = "Spot price must be positive"
            }
      when (input.strike <= 0) $
        Servant.throwError
          Servant.err400
            { Servant.errBody = "Strike price must be positive"
            }
      when (getTimeToExpiryDays input.timeToExpiry <= 0) $
        Servant.throwError
          Servant.err400
            { Servant.errBody = "Time to expiry must be positive"
            }
      when (input.volatility <= 0) $
        Servant.throwError
          Servant.err400
            { Servant.errBody = "Volatility must be positive"
            }
      when (input.riskFreeRate < 0) $
        Servant.throwError
          Servant.err400
            { Servant.errBody = "Risk-free rate must be non-negative"
            }
      -- Check all values are finite (not NaN or Infinity)
      unless
        ( isFinite input.spot
            && isFinite input.strike
            && isFinite (getTimeToExpiryDays input.timeToExpiry)
            && isFinite input.volatility
            && isFinite input.riskFreeRate
        )
        $ Servant.throwError
          Servant.err400
            { Servant.errBody = "All input values must be finite (not NaN or Infinity)"
            }
      -- Calculate price with Greeks
      pure $ BlackScholes.calculatePriceWithGreeks input

    isFinite :: Double -> Bool
    isFinite x = not (isNaN x || isInfinite x)


app :: Wai.Application
app =
  Server.serve
    (Servant.Proxy :: Servant.Proxy (NamedRoutes API))
    server


-- | Generate OpenAPI spec by running: stack exec generate-openapi
-- This creates openapi.json at project root for TypeScript client generation.
apiOpenApi :: OpenApi.OpenApi
apiOpenApi =
  Servant.OpenApi.toOpenApi (Servant.Proxy :: Servant.Proxy (NamedRoutes API))
    & OpenApi.info . OpenApi.title .~ "Quanty API"
    & OpenApi.info . OpenApi.version .~ "0.1.0"
    & OpenApi.info . OpenApi.description
      ?~ "Options pricing and financial derivatives API"
