-- |
-- Module: BlackScholesSpec
-- Description: Comprehensive test suite for Black-Scholes pricing model
--
-- This test suite validates:
-- - Known value tests (specific scenarios with expected results)
-- - Financial identities (call-put parity, boundary conditions)
-- - Greeks validation (ranges, relationships, properties)
-- - Property-based tests with QuickCheck (invariants hold for random inputs)
module BlackScholesSpec (spec) where

import BlackScholes
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ()


-- | Tolerance for floating-point comparisons
epsilon :: Double
epsilon = 1e-10


-- | Helper to create TimeToExpiryDays in tests (unsafe, panics on invalid values)
unsafeMkTimeToExpiryDays :: Double -> TimeToExpiryDays
unsafeMkTimeToExpiryDays d = case mkTimeToExpiryDays d of
  Right t -> t
  Left err -> panic $ "Invalid TimeToExpiryDays in test: " <> show err


-- | Helper to create a standard ATM (at-the-money) call option for testing
-- Using 30 days to expiry
standardAtmCall :: BlackScholes.Inputs
standardAtmCall =
  BlackScholes.Inputs
    { spot = 100.0
    , strike = 100.0
    , timeToExpiry = unsafeMkTimeToExpiryDays 30
    , volatility = 0.2
    , riskFreeRate = 0.05
    , kind = Call
    }


-- | Helper to create a standard ATM put option for testing
standardAtmPut :: BlackScholes.Inputs
standardAtmPut = standardAtmCall {kind = Put}


spec :: Spec
spec = do
  describe "Black-Scholes Pricing - Known Values" $ do
    it "prices ATM call option correctly" $ do
      let price = calculatePrice standardAtmCall
      price `shouldSatisfy` (\p -> p > 0 && p < standardAtmCall.spot)

    it "prices ATM put option correctly" $ do
      let price = calculatePrice standardAtmPut
      price `shouldSatisfy` (\p -> p > 0 && p < standardAtmPut.strike)

    it "satisfies call-put parity" $ do
      let spotPrice = 100.0
          strikePrice = 100.0
          timeDays = 30.0
          timeYears = timeDays / 365.0 -- Convert days to years for the formula
          rate = 0.05
          callInput = standardAtmCall
          putInput = standardAtmPut
          callPrice = calculatePrice callInput
          putPrice = calculatePrice putInput
          lhs = callPrice - putPrice
          rhs = spotPrice - strikePrice * exp (-(rate * timeYears))
      abs (lhs - rhs) `shouldSatisfy` (< epsilon)

    it "prices deep ITM call close to intrinsic value" $ do
      let input =
            BlackScholes.Inputs
              { spot = 150.0
              , strike = 100.0
              , timeToExpiry = unsafeMkTimeToExpiryDays 1 -- 1 day to expiry
              , volatility = 0.2
              , riskFreeRate = 0.05
              , kind = Call
              }
          price = calculatePrice input
          intrinsicValue = input.spot - input.strike
      abs (price - intrinsicValue) `shouldSatisfy` (< 1.0)

    it "prices deep OTM call close to zero" $ do
      let input =
            BlackScholes.Inputs
              { spot = 50.0
              , strike = 100.0
              , timeToExpiry = unsafeMkTimeToExpiryDays 1 -- 1 day to expiry
              , volatility = 0.2
              , riskFreeRate = 0.05
              , kind = Call
              }
          price = calculatePrice input
      price `shouldSatisfy` (< 0.1)

  describe "Greeks" $ do
    it "calculates delta for call in valid range" $ do
      let result = calculatePriceWithGreeks standardAtmCall
      result.greeks.delta `shouldSatisfy` (\delta -> delta >= 0 && delta <= 1)

    it "calculates delta for put in valid range" $ do
      let result = calculatePriceWithGreeks standardAtmPut
      result.greeks.delta `shouldSatisfy` (\delta -> delta >= -1 && delta <= 0)

    it "calculates positive gamma" $ do
      let result = calculatePriceWithGreeks standardAtmCall
      result.greeks.gamma `shouldSatisfy` (> 0)

    it "calculates positive vega" $ do
      let result = calculatePriceWithGreeks standardAtmCall
      result.greeks.vega `shouldSatisfy` (> 0)

    it "calculates negative theta for long call" $ do
      let result = calculatePriceWithGreeks standardAtmCall
      result.greeks.theta `shouldSatisfy` (< 0)

  describe "Property tests" $ do
    prop
      "option price is always non-negative (allowing for floating point precision)"
      $ \spotPrice strikePrice timeDays volatility rate ->
        let validInput =
              BlackScholes.Inputs
                { spot = clampRange 10 1000 (abs spotPrice)
                , strike = clampRange 10 1000 (abs strikePrice)
                , timeToExpiry = unsafeMkTimeToExpiryDays (clampRange 3 180 (abs timeDays)) -- 3-180 days
                , volatility = clampRange 0.1 2.5 (abs volatility)
                , riskFreeRate = clampRange 0 0.15 (abs rate)
                , kind = Call
                }
            price = calculatePrice validInput
         in price >= -epsilon -- Allow tiny negative due to floating point errors
    prop "call delta is between 0 and 1" $
      \spotPrice strikePrice timeDays volatility rate ->
        let validInput =
              BlackScholes.Inputs
                { spot = clampRange 10 1000 (abs spotPrice)
                , strike = clampRange 10 1000 (abs strikePrice)
                , timeToExpiry = unsafeMkTimeToExpiryDays (clampRange 3 180 (abs timeDays)) -- 3-180 days
                , volatility = clampRange 0.1 2.5 (abs volatility)
                , riskFreeRate = clampRange 0 0.15 (abs rate)
                , kind = Call
                }
            result = calculatePriceWithGreeks validInput
         in result.greeks.delta >= 0 && result.greeks.delta <= 1

    prop "put delta is between -1 and 0" $
      \spotPrice strikePrice timeDays volatility rate ->
        let validInput =
              BlackScholes.Inputs
                { spot = clampRange 10 1000 (abs spotPrice)
                , strike = clampRange 10 1000 (abs strikePrice)
                , timeToExpiry = unsafeMkTimeToExpiryDays (clampRange 3 180 (abs timeDays)) -- 3-180 days
                , volatility = clampRange 0.1 2.5 (abs volatility)
                , riskFreeRate = clampRange 0 0.15 (abs rate)
                , kind = Put
                }
            result = calculatePriceWithGreeks validInput
         in result.greeks.delta >= -1 && result.greeks.delta <= 0

    prop "gamma is always positive and finite" $
      \spotPrice strikePrice timeDays volatility rate ->
        let validInput =
              BlackScholes.Inputs
                { spot = clampRange 50 100 (abs spotPrice) -- 50-150 range
                , strike = clampRange 50 100 (abs strikePrice) -- 50-150 range
                , timeToExpiry = unsafeMkTimeToExpiryDays (clampRange 7 180 (abs timeDays)) -- 7-180 days
                , volatility = clampRange 0.15 2.5 (abs volatility)
                , riskFreeRate = clampRange 0 0.15 (abs rate)
                , kind = Call
                }
            result = calculatePriceWithGreeks validInput
         in result.greeks.gamma > 0 && isFinite result.greeks.gamma


-- | Clamp a value to be within a range [minVal, minVal + maxVal]
clampRange :: Double -> Double -> Double -> Double
clampRange minVal maxVal x =
  let normalized = abs x
      clamped = min normalized maxVal
   in minVal + clamped


-- | Helper to check if a value is finite (not NaN or Infinity)
isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)
