-- |
-- Module: BlackScholes
-- Description: Black-Scholes model for European options pricing
-- Copyright: (c) 2025
--
-- This module implements the Black-Scholes-Merton model for pricing European-style
-- options. The model provides closed-form solutions for option prices and the Greeks
-- (sensitivities to various parameters).
--
-- The Black-Scholes model assumes:
-- - European exercise (only at expiry)
-- - No dividends
-- - Constant volatility and risk-free rate
-- - Log-normal distribution of underlying price
-- - Frictionless markets (no transaction costs)
--
-- For cryptocurrency markets, note that we use 365 days/year due to 24/7 trading.
module BlackScholes (
  -- * Core Types
  Inputs (..),
  Greeks (..),
  OptionPrice (..),

  -- * Pricing Functions
  calculatePrice,
  calculatePriceWithGreeks,
  calculateD1,
  calculateD2,
  calculateCallPrice,
  calculatePutPrice,

  -- * Re-exports from Option
  module Option,
) where

import Data.Aeson qualified as Aeson
import Data.Number.Erf (erf)
import Data.OpenApi (ToSchema)
import Option
import Protolude


-- | Input parameters for Black-Scholes pricing.
--
-- All parameters must be positive and finite. Violations will result in
-- incorrect pricing (NaN or Infinity).
data Inputs = Inputs
  { spot :: Double
  -- ^ Current price of the underlying asset (S). Must be > 0.
  , strike :: Double
  -- ^ Strike price of the option (K). Must be > 0.
  , timeToExpiry :: TimeToExpiryDays
  -- ^ Time to expiration in days. Must be > 0.
  , volatility :: Double
  -- ^ Annualized volatility (sigma) as a decimal. Must be > 0.
  -- For example, 25% volatility = 0.25.
  , riskFreeRate :: Double
  -- ^ Risk-free interest rate (r) as a decimal. Must be >= 0.
  -- For example, 5% rate = 0.05.
  , kind :: OptionKind
  -- ^ Kind of option (Call or Put).
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


-- | The Greeks measure the sensitivity of option price to various parameters.
--
-- All Greeks are computed analytically from the Black-Scholes formula.
data Greeks = Greeks
  { delta :: Double
  -- ^ Rate of change of option price with respect to underlying price (dV/dS).
  -- Range: [0, 1] for calls, [-1, 0] for puts.
  -- Interpretation: A delta of 0.5 means the option price changes by ~$0.50
  -- for every $1 change in the underlying.
  , gamma :: Double
  -- ^ Rate of change of delta with respect to underlying price (d^2V/dS^2).
  -- Always positive. Highest for ATM options, lower for ITM/OTM.
  -- Interpretation: Measures convexity and how quickly delta changes.
  , vega :: Double
  -- ^ Rate of change of option price with respect to volatility (dV/d_sigma).
  -- Always positive (higher volatility increases option value).
  -- Expressed per 1% change in volatility.
  -- Interpretation: A vega of 0.15 means option price increases ~$0.15
  -- if volatility increases from 20% to 21%.
  , theta :: Double
  -- ^ Rate of change of option price with respect to time (dV/dt).
  -- Usually negative (time decay). Expressed per day.
  -- Interpretation: A theta of -0.05 means the option loses ~$0.05
  -- in value per day (all else equal).
  , rho :: Double
  -- ^ Rate of change of option price with respect to risk-free rate (dV/dr).
  -- Positive for calls, negative for puts. Expressed per 1% change in rate.
  -- Interpretation: A rho of 0.10 means option price increases ~$0.10
  -- if interest rates increase from 5% to 6%.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


-- | Result of Black-Scholes pricing calculation.
--
-- Contains both the fair value price and all five standard Greeks.
data OptionPrice = OptionPrice
  { price :: Double
  -- ^ Fair value price of the option in the same units as the underlying.
  , greeks :: Greeks
  -- ^ Sensitivities (Greeks) of the option price to various parameters.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


-- | Calculate the fair value price of a European option using
-- the Black-Scholes-Merton model.
--
-- This function computes only the option price without Greeks.
-- For pricing with Greeks, use 'calculatePriceWithGreeks'.
--
-- ==== __Example__
-- @
-- let input = Inputs
--       { spot = 100.0
--       , strike = 105.0
--       , timeToExpiry = TimeToExpiryDays 30  -- 30 days
--       , volatility = 0.20                   -- 20% annual volatility
--       , riskFreeRate = 0.05                 -- 5% annual rate
--       , kind = Call
--       }
--     price = calculatePrice input
-- @
--
-- ==== __Preconditions__
-- * @spot > 0@ - Underlying price must be positive
-- * @strike > 0@ - Strike price must be positive
-- * @timeToExpiry > 0@ - Must not be expired
-- * @volatility > 0@ - Volatility must be positive
-- * All values must be finite (not NaN or Infinity)
--
-- Violating these preconditions will result in NaN or Infinity in the output.
calculatePrice :: Inputs -> Double
calculatePrice input =
  let d1Value = calculateD1 input
      d2Value = calculateD2 input d1Value
   in case input.kind of
        Call -> calculateCallPrice input d1Value d2Value
        Put -> calculatePutPrice input d1Value d2Value


-- | Calculate the fair value price and Greeks for a European option using
-- the Black-Scholes-Merton model.
--
-- This function computes both the option price and all five standard Greeks
-- in a single pass for efficiency.
--
-- ==== __Example__
-- @
-- let input = Inputs
--       { spot = 100.0
--       , strike = 105.0
--       , timeToExpiry = TimeToExpiryDays 30  -- 30 days
--       , volatility = 0.20                   -- 20% annual volatility
--       , riskFreeRate = 0.05                 -- 5% annual rate
--       , kind = Call
--       }
--     result = calculatePriceWithGreeks input
-- @
--
-- ==== __Preconditions__
-- * @spot > 0@ - Underlying price must be positive
-- * @strike > 0@ - Strike price must be positive
-- * @timeToExpiry > 0@ - Must not be expired
-- * @volatility > 0@ - Volatility must be positive
-- * All values must be finite (not NaN or Infinity)
--
-- Violating these preconditions will result in NaN or Infinity in the output.
calculatePriceWithGreeks :: Inputs -> OptionPrice
calculatePriceWithGreeks input = OptionPrice {..}
  where
    d1Value = calculateD1 input
    d2Value = calculateD2 input d1Value

    price = case input.kind of
      Call -> calculateCallPrice input d1Value d2Value
      Put -> calculatePutPrice input d1Value d2Value

    greeks = calculateGreeks input d1Value d2Value


-- | Calculate the d1 term in the Black-Scholes formula.
--
-- The d1 term represents a standardized measure combining moneyness (how far
-- the spot price is from the strike), expected drift from the risk-free rate,
-- and volatility effects. It is used to determine N(d1), the hedge ratio for
-- calls, and appears in both pricing formulas and Greeks calculations.
--
-- Formula:
-- @
-- d1 = (ln(S/K) + (r + sigma^2/2) * T) / (sigma * sqrt(T))
-- @
--
-- Where:
-- * S = spot price (current price of the underlying asset)
-- * K = strike price (the price at which the option can be exercised)
-- * r = risk-free interest rate (annualized, as a decimal)
-- * sigma = volatility (annualized standard deviation of returns, as a decimal)
-- * T = time to expiry (in years)
-- * ln = natural logarithm
--
-- The numerator has two components:
-- 1. ln(S/K): The log-moneyness, measuring how far the spot is from strike
-- 2. (r + sigma^2/2) * T: The expected drift adjusted for volatility
--
-- The denominator (sigma * sqrt(T)) normalizes by volatility over time.
calculateD1 :: Inputs -> Double
calculateD1 input =
  let tYears = getTimeToExpiryDays input.timeToExpiry / 365.0 -- Convert days to years
   in ( log (input.spot / input.strike)
          + (input.riskFreeRate + 0.5 * input.volatility ** 2) * tYears
      )
        / (input.volatility * sqrt tYears)


-- | Calculate the d2 term in the Black-Scholes formula.
--
-- The d2 term is derived from d1 by subtracting one standard deviation of
-- returns (sigma * sqrt(T)). In the risk-neutral framework, N(d2) represents
-- the probability that the option will be exercised at expiration - that is,
-- the probability that the option will expire in-the-money.
--
-- Formula:
-- @
-- d2 = d1 - sigma * sqrt(T)
-- @
--
-- Where:
-- * d1 = the d1 term from calculateD1
-- * sigma = volatility (annualized standard deviation of returns)
-- * T = time to expiry (in years)
--
-- The difference between d1 and d2 (sigma * sqrt(T)) represents one standard
-- deviation of the log-returns over the remaining time to expiration.
calculateD2 :: Inputs -> Double -> Double
calculateD2 input d1Value =
  let tYears = getTimeToExpiryDays input.timeToExpiry / 365.0
   in d1Value - input.volatility * sqrt tYears


-- | Calculate the fair value of a European call option.
--
-- Uses the Black-Scholes formula for call options.
--
-- Formula:
-- @
-- C = S * N(d1) - K * e^(-r*T) * N(d2)
-- @
--
-- Where:
-- * C = call option price
-- * S = spot price of the underlying
-- * K = strike price
-- * r = risk-free interest rate
-- * T = time to expiry (in years)
-- * N(x) = cumulative standard normal distribution function
-- * d1, d2 = intermediate values from calculateD1 and calculateD2
--
-- This represents the expected value of the call payoff under risk-neutral
-- pricing, discounted to present value.
calculateCallPrice :: Inputs -> Double -> Double -> Double
calculateCallPrice input d1Value d2Value =
  let tYears = getTimeToExpiryDays input.timeToExpiry / 365.0
      discountFactor = exp (-(input.riskFreeRate * tYears))
   in input.spot * standardNormalCdf d1Value
        - input.strike * discountFactor * standardNormalCdf d2Value


-- | Calculate the fair value of a European put option.
--
-- Uses the Black-Scholes formula for put options, which can be derived
-- from call-put parity or directly from the model.
--
-- Formula:
-- @
-- P = K * e^(-r*T) * N(-d2) - S * N(-d1)
-- @
--
-- Where:
-- * P = put option price
-- * S = spot price of the underlying
-- * K = strike price
-- * r = risk-free interest rate
-- * T = time to expiry (in years)
-- * N(x) = cumulative standard normal distribution function
-- * d1, d2 = intermediate values from calculateD1 and calculateD2
--
-- This represents the expected value of the put payoff under risk-neutral
-- pricing, discounted to present value.
calculatePutPrice :: Inputs -> Double -> Double -> Double
calculatePutPrice input d1Value d2Value =
  let tYears = getTimeToExpiryDays input.timeToExpiry / 365.0
      discountFactor = exp (-(input.riskFreeRate * tYears))
   in input.strike * discountFactor * standardNormalCdf (-d2Value)
        - input.spot * standardNormalCdf (-d1Value)


-- | Cumulative distribution function (CDF) of the standard normal distribution.
--
-- This is N(x) where N ~ Normal(0, 1).
--
-- Uses the error function (erf) for accurate computation:
-- @
-- N(x) = Phi(x) = (1/2) * (1 + erf(x / sqrt(2)))
-- @
--
-- Where:
-- * Phi(x) = cumulative distribution function of standard normal
-- * erf = error function
-- * sqrt(2) = square root of 2
standardNormalCdf :: Double -> Double
standardNormalCdf x = 0.5 * (1.0 + erf (x / sqrt 2.0))


-- | Calculate all five standard Greeks for the option.
--
-- The Greeks are the partial derivatives of the option price with respect
-- to various parameters. They are computed analytically for efficiency.
--
-- The calculations reuse d1 and d2 from the pricing computation to avoid
-- redundant calculations.
--
-- Note: Vega, theta, and rho are scaled to represent:
-- - Vega: Change per 1% point of volatility (e.g., 20% -> 21%)
-- - Theta: Change per calendar day (annual rate / 365)
-- - Rho: Change per 1% point of interest rate (e.g., 5% -> 6%)
calculateGreeks :: Inputs -> Double -> Double -> Greeks
calculateGreeks input d1Value d2Value = Greeks {..}
  where
    -- Standard normal PDF at d1, used in multiple Greeks
    normalPdfAtD1 = standardNormalPdf d1Value
    tYears = getTimeToExpiryDays input.timeToExpiry / 365.0 -- Convert days to years
    discountFactor = exp (-(input.riskFreeRate * tYears))
    sqrtTimeToExpiry = sqrt tYears

    -- Delta: dV/dS
    -- Measures how much option price changes with underlying price
    -- Call delta: N(d1), Put delta: N(d1) - 1
    delta = case input.kind of
      Call -> standardNormalCdf d1Value
      Put -> standardNormalCdf d1Value - 1.0

    -- Gamma: d^2V/dS^2
    -- Measures rate of change of delta (same for calls and puts)
    -- Highest for ATM options, lower for ITM/OTM
    gamma = normalPdfAtD1 / (input.spot * input.volatility * sqrtTimeToExpiry)

    -- Vega: dV/d_sigma
    -- Measures sensitivity to volatility (same for calls and puts)
    -- Scaled to represent change per 1% point (divide by 100)
    vega = input.spot * normalPdfAtD1 * sqrtTimeToExpiry / 100.0

    -- Theta: dV/dt (but displayed as -dV/dt for time decay)
    -- Measures time decay - how much value is lost per day
    -- Usually negative (options lose value as time passes)
    theta = case input.kind of
      Call ->
        let volDecayTerm = -(input.spot * normalPdfAtD1 * input.volatility / (2.0 * sqrtTimeToExpiry))
            interestTerm =
              -(input.riskFreeRate * input.strike * discountFactor * standardNormalCdf d2Value)
         in (volDecayTerm + interestTerm) / 365.0 -- Per day
      Put ->
        let volDecayTerm = -(input.spot * normalPdfAtD1 * input.volatility / (2.0 * sqrtTimeToExpiry))
            interestTerm =
              input.riskFreeRate
                * input.strike
                * discountFactor
                * standardNormalCdf (-d2Value)
         in (volDecayTerm + interestTerm) / 365.0 -- Per day

    -- Rho: dV/dr
    -- Measures sensitivity to interest rate changes
    -- Positive for calls, negative for puts
    -- Scaled to represent change per 1% point (divide by 100)
    rho = case input.kind of
      Call ->
        input.strike * tYears * discountFactor * standardNormalCdf d2Value / 100.0
      Put ->
        -(input.strike * tYears * discountFactor * standardNormalCdf (-d2Value) / 100.0)


-- | Probability density function (PDF) of the standard normal distribution.
--
-- This is phi(x) where phi is the PDF of Normal(0, 1).
--
-- Formula:
-- @
-- phi(x) = (1 / sqrt(2 * pi)) * e^(-x^2 / 2)
-- @
--
-- Where:
-- * phi(x) = probability density function of standard normal
-- * pi = mathematical constant pi (3.14159...)
-- * e = Euler's number (2.71828...)
--
-- Used in calculating Greeks (particularly gamma and vega).
standardNormalPdf :: Double -> Double
standardNormalPdf x = exp (-(0.5 * x ** 2)) / sqrt (2.0 * pi)
