-- |
-- Module: Option
-- Description: Common types and concepts for options pricing
--
-- This module contains fundamental option types shared across different
-- pricing models (Black-Scholes, binomial, Monte Carlo, etc.).
module Option (
  -- * Option Types
  OptionKind (..),
  TimeToExpiryDays,
  TimeToExpiryError (..),

  -- * Smart Constructors
  mkTimeToExpiryDays,
  getTimeToExpiryDays,
) where

import Control.Monad.Fail qualified as Fail
import Data.Aeson qualified as Aeson
import Data.OpenApi (ToSchema)
import Protolude


-- | Type of option contract.
--
-- 'Call' gives the right to buy the underlying at the strike price.
-- 'Put' gives the right to sell the underlying at the strike price.
data OptionKind
  = Call
  | Put
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


-- | Errors that can occur when constructing 'TimeToExpiryDays'.
data TimeToExpiryError
  = -- | The value was NaN or Infinity
    TimeToExpiryNotFinite
  | -- | The value was zero or negative
    TimeToExpiryNotPositive
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


-- | Time to expiration expressed in days.
--
-- This newtype makes the units explicit and is natural for cryptocurrency
-- options which typically expire in days or weeks rather than years.
--
-- Use 'mkTimeToExpiryDays' to construct values safely.
--
-- ==== __Examples__
-- @
-- mkTimeToExpiryDays 1    -- Right (TimeToExpiryDays 1)
-- mkTimeToExpiryDays 7    -- Right (TimeToExpiryDays 7)
-- mkTimeToExpiryDays 30   -- Right (TimeToExpiryDays 30)
-- mkTimeToExpiryDays 90   -- Right (TimeToExpiryDays 90)
-- mkTimeToExpiryDays (-5) -- Left TimeToExpiryNotPositive
-- mkTimeToExpiryDays 0    -- Left TimeToExpiryNotPositive
-- @
--
-- Internally converted to years for pricing formulas using
-- 365 days/year (crypto markets trade 24/7).
newtype TimeToExpiryDays = TimeToExpiryDays
  { days :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSchema)


-- | Custom ToJSON instance that serializes to just the days value
instance Aeson.ToJSON TimeToExpiryDays where
  toJSON (TimeToExpiryDays daysValue) = Aeson.toJSON daysValue


-- | Custom FromJSON instance that validates using the smart constructor
instance Aeson.FromJSON TimeToExpiryDays where
  parseJSON = Aeson.withScientific "TimeToExpiryDays" $ \scientific ->
    let daysValue = realToFrac scientific
     in case mkTimeToExpiryDays daysValue of
          Left TimeToExpiryNotFinite -> Fail.fail "Time to expiry must be finite (not NaN or Infinity)"
          Left TimeToExpiryNotPositive -> Fail.fail "Time to expiry must be positive"
          Right timeToExpiry -> pure timeToExpiry


-- | Smart constructor for 'TimeToExpiryDays'.
--
-- Validates that the time to expiry is positive and finite.
mkTimeToExpiryDays :: Double -> Either TimeToExpiryError TimeToExpiryDays
mkTimeToExpiryDays daysValue
  | isNaN daysValue || isInfinite daysValue = Left TimeToExpiryNotFinite
  | daysValue <= 0 = Left TimeToExpiryNotPositive
  | otherwise = Right (TimeToExpiryDays daysValue)


-- | Extract the time in days from 'TimeToExpiryDays'.
getTimeToExpiryDays :: TimeToExpiryDays %1 -> Double
getTimeToExpiryDays (TimeToExpiryDays daysValue) = daysValue
