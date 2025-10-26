{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: BlackScholes
-- Description: Black-Scholes model for European options pricing (WASM version)
--
-- This is the WASM version of the BlackScholes module, containing all types
-- and calculation logic in a single module (package by feature).
--
-- All types derive TypeScript instances for automatic type generation.
module BlackScholes (
  -- * Option Types
  OptionKind (..),
  TimeToExpiryDays (..),

  -- * Input and Output Types
  Inputs (..),
  Greeks (..),
  OptionPrice (..),

  -- * Pricing Functions
  calculatePrice,
  calculatePriceWithGreeks,
) where

import Data.Aeson (FromJSON, Options, ToJSON, defaultOptions)
import Data.Aeson.TypeScript.TH qualified as TS
import Data.Number.Erf (erf)
import Data.Text (Text)
import GHC.Generics (Generic)


-- | Type of option contract.
data OptionKind
  = Call
  | Put
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


$(TS.deriveTypeScript defaultOptions ''OptionKind)


-- | Time to expiration expressed in days.
--
-- For WASM, we use a simple newtype without validation.
-- Validation will happen on the TypeScript side.
newtype TimeToExpiryDays = TimeToExpiryDays
  { days :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


$(TS.deriveTypeScript defaultOptions ''TimeToExpiryDays)


-- | Input parameters for Black-Scholes pricing.
data Inputs = Inputs
  { spot :: Double
  , strike :: Double
  , timeToExpiry :: TimeToExpiryDays
  , volatility :: Double
  , riskFreeRate :: Double
  , kind :: OptionKind
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


$(TS.deriveTypeScript defaultOptions ''Inputs)


-- | The Greeks measure the sensitivity of option price to various parameters.
data Greeks = Greeks
  { delta :: Double
  , gamma :: Double
  , vega :: Double
  , theta :: Double
  , rho :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


$(TS.deriveTypeScript defaultOptions ''Greeks)


-- | Result of Black-Scholes pricing calculation.
data OptionPrice = OptionPrice
  { price :: Double
  , greeks :: Greeks
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


$(TS.deriveTypeScript defaultOptions ''OptionPrice)


-- | Calculate the fair value price of a European option.
calculatePrice :: Inputs -> Double
calculatePrice input =
  let d1Value = calculateD1 input
      d2Value = calculateD2 input d1Value
   in case kind input of
        Call -> calculateCallPrice input d1Value d2Value
        Put -> calculatePutPrice input d1Value d2Value


-- | Calculate the fair value price and Greeks for a European option.
calculatePriceWithGreeks :: Inputs -> OptionPrice
calculatePriceWithGreeks input = OptionPrice priceValue greeksValue
  where
    d1Value = calculateD1 input
    d2Value = calculateD2 input d1Value

    priceValue = case kind input of
      Call -> calculateCallPrice input d1Value d2Value
      Put -> calculatePutPrice input d1Value d2Value

    greeksValue = calculateGreeks input d1Value d2Value


-- | Calculate the d1 term in the Black-Scholes formula.
calculateD1 :: Inputs -> Double
calculateD1 input =
  let tYears = days (timeToExpiry input) / 365.0
   in ( log (spot input / strike input)
          + (riskFreeRate input + 0.5 * volatility input ** 2) * tYears
      )
        / (volatility input * sqrt tYears)


-- | Calculate the d2 term in the Black-Scholes formula.
calculateD2 :: Inputs -> Double -> Double
calculateD2 input d1Value =
  let tYears = days (timeToExpiry input) / 365.0
   in d1Value - volatility input * sqrt tYears


-- | Calculate the fair value of a European call option.
calculateCallPrice :: Inputs -> Double -> Double -> Double
calculateCallPrice input d1Value d2Value =
  let tYears = days (timeToExpiry input) / 365.0
      discountFactor = exp (-(riskFreeRate input * tYears))
   in spot input * standardNormalCdf d1Value
        - strike input * discountFactor * standardNormalCdf d2Value


-- | Calculate the fair value of a European put option.
calculatePutPrice :: Inputs -> Double -> Double -> Double
calculatePutPrice input d1Value d2Value =
  let tYears = days (timeToExpiry input) / 365.0
      discountFactor = exp (-(riskFreeRate input * tYears))
   in strike input * discountFactor * standardNormalCdf (-d2Value)
        - spot input * standardNormalCdf (-d1Value)


-- | Cumulative distribution function of the standard normal distribution.
standardNormalCdf :: Double -> Double
standardNormalCdf x = 0.5 * (1.0 + erf (x / sqrt 2.0))


-- | Calculate all five standard Greeks for the option.
calculateGreeks :: Inputs -> Double -> Double -> Greeks
calculateGreeks input d1Value d2Value =
  Greeks
    { delta = deltaValue
    , gamma = gammaValue
    , vega = vegaValue
    , theta = thetaValue
    , rho = rhoValue
    }
  where
    normalPdfAtD1 = standardNormalPdf d1Value
    tYears = days (timeToExpiry input) / 365.0
    discountFactor = exp (-(riskFreeRate input * tYears))
    sqrtTimeToExpiry = sqrt tYears

    deltaValue = case kind input of
      Call -> standardNormalCdf d1Value
      Put -> standardNormalCdf d1Value - 1.0

    gammaValue = normalPdfAtD1 / (spot input * volatility input * sqrtTimeToExpiry)

    vegaValue = spot input * normalPdfAtD1 * sqrtTimeToExpiry / 100.0

    thetaValue = case kind input of
      Call ->
        let volDecayTerm = -(spot input * normalPdfAtD1 * volatility input / (2.0 * sqrtTimeToExpiry))
            interestTerm =
              -(riskFreeRate input * strike input * discountFactor * standardNormalCdf d2Value)
         in (volDecayTerm + interestTerm) / 365.0
      Put ->
        let volDecayTerm = -(spot input * normalPdfAtD1 * volatility input / (2.0 * sqrtTimeToExpiry))
            interestTerm =
              riskFreeRate input
                * strike input
                * discountFactor
                * standardNormalCdf (-d2Value)
         in (volDecayTerm + interestTerm) / 365.0

    rhoValue = case kind input of
      Call ->
        strike input * tYears * discountFactor * standardNormalCdf d2Value / 100.0
      Put ->
        -(strike input * tYears * discountFactor * standardNormalCdf (-d2Value) / 100.0)


-- | Probability density function of the standard normal distribution.
standardNormalPdf :: Double -> Double
standardNormalPdf x = exp (-(0.5 * x ** 2)) / sqrt (2.0 * pi)
