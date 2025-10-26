module ApiSpec (spec) where

import Api qualified
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Network.Wai.Test qualified as Wai
import Protolude
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
  shouldSatisfy,
 )
import Test.Hspec.Wai (ResponseMatcher (matchStatus))
import Test.Hspec.Wai qualified as Hspec
import Test.Hspec.Wai.JSON qualified as Hspec


spec :: Spec
spec = Hspec.with (pure Api.app) $ do
  describe "GET /health" $ do
    it "responds with 200" $ do
      Hspec.get "/health" `Hspec.shouldRespondWith` 200

    it "returns valid health status" $ do
      let expected = [Hspec.json|{status: "ok", version: "0.1.0"}|] {matchStatus = 200}
      Hspec.get "/health" `Hspec.shouldRespondWith` expected

    it "returns JSON with status and version fields" $ do
      response <- Hspec.get "/health"
      let maybeJson = Aeson.decode @Aeson.Value (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid JSON"
        Just (Aeson.Object obj) -> do
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "status") o)
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "version") o)
        Just _ -> expectationFailure "Response is not a JSON object"

  describe "GET /placeholder" $ do
    it "responds with 200" $ do
      Hspec.get "/placeholder" `Hspec.shouldRespondWith` 200

    it "returns JSON with message and timestamp fields" $ do
      response <- Hspec.get "/placeholder"
      let maybeJson = Aeson.decode @Aeson.Value (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid JSON"
        Just (Aeson.Object obj) -> do
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "message") o)
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "timestamp") o)
        Just _ -> expectationFailure "Response is not a JSON object"

    it "returns expected message text" $ do
      response <- Hspec.get "/placeholder"
      let maybeJson = Aeson.decode @Api.PlaceholderResponse (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid PlaceholderResponse"
        Just placeholderResp ->
          placeholderResp.message `shouldBe` "Hello from Quanty API"

  describe "POST /black-scholes" $ do
    it "responds with 200 for valid input" $ do
      let validInput =
            [Hspec.json|{
              spot: 100,
              strike: 105,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        validInput
        `Hspec.shouldRespondWith` 200

    it "returns option price with greeks" $ do
      let validInput =
            [Hspec.json|{
              spot: 100,
              strike: 100,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      response <-
        Hspec.request
          "POST"
          "/black-scholes"
          [("Content-Type", "application/json;charset=utf-8")]
          validInput
      let maybeJson = Aeson.decode @Aeson.Value (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid JSON"
        Just (Aeson.Object obj) -> do
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "price") o)
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "greeks") o)
        Just _ -> expectationFailure "Response is not a JSON object"

    it "returns positive price for ATM call" $ do
      let validInput =
            [Hspec.json|{
              spot: 100,
              strike: 100,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      response <-
        Hspec.request
          "POST"
          "/black-scholes"
          [("Content-Type", "application/json;charset=utf-8")]
          validInput
      let maybeJson = Aeson.decode @Aeson.Value (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid JSON"
        Just (Aeson.Object obj) -> do
          case KeyMap.lookup (Key.fromString "price") obj of
            Just (Aeson.Number price) -> (realToFrac price :: Double) `shouldSatisfy` (> 0)
            _ -> expectationFailure "Price field missing or not a number"
        Just _ -> expectationFailure "Response is not a JSON object"

    it "rejects negative spot price" $ do
      let invalidInput =
            [Hspec.json|{
              spot: -100,
              strike: 105,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        invalidInput
        `Hspec.shouldRespondWith` 400

    it "rejects negative strike price" $ do
      let invalidInput =
            [Hspec.json|{
              spot: 100,
              strike: -105,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        invalidInput
        `Hspec.shouldRespondWith` 400

    it "rejects zero time to expiry" $ do
      let invalidInput =
            [Hspec.json|{
              spot: 100,
              strike: 105,
              timeToExpiry: { days: 0 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        invalidInput
        `Hspec.shouldRespondWith` 400

    it "rejects negative time to expiry" $ do
      let invalidInput =
            [Hspec.json|{
              spot: 100,
              strike: 105,
              timeToExpiry: { days: -30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        invalidInput
        `Hspec.shouldRespondWith` 400

    it "rejects negative volatility" $ do
      let invalidInput =
            [Hspec.json|{
              spot: 100,
              strike: 105,
              timeToExpiry: { days: 30 },
              volatility: -0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        invalidInput
        `Hspec.shouldRespondWith` 400

    it "rejects negative risk-free rate" $ do
      let invalidInput =
            [Hspec.json|{
              spot: 100,
              strike: 105,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: -0.05,
              kind: "Call"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        invalidInput
        `Hspec.shouldRespondWith` 400

    it "accepts both Call and Put option kinds" $ do
      let callInput =
            [Hspec.json|{
              spot: 100,
              strike: 105,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      let putInput =
            [Hspec.json|{
              spot: 100,
              strike: 105,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Put"
            }|]
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        callInput
        `Hspec.shouldRespondWith` 200
      Hspec.request
        "POST"
        "/black-scholes"
        [("Content-Type", "application/json;charset=utf-8")]
        putInput
        `Hspec.shouldRespondWith` 200

    it "returns all five greeks" $ do
      let validInput =
            [Hspec.json|{
              spot: 100,
              strike: 100,
              timeToExpiry: { days: 30 },
              volatility: 0.2,
              riskFreeRate: 0.05,
              kind: "Call"
            }|]
      response <-
        Hspec.request
          "POST"
          "/black-scholes"
          [("Content-Type", "application/json;charset=utf-8")]
          validInput
      let maybeJson = Aeson.decode @Aeson.Value (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid JSON"
        Just (Aeson.Object obj) -> do
          case KeyMap.lookup (Key.fromString "greeks") obj of
            Just (Aeson.Object greeks) -> do
              greeks `shouldSatisfy` \g -> isJust (KeyMap.lookup (Key.fromString "delta") g)
              greeks `shouldSatisfy` \g -> isJust (KeyMap.lookup (Key.fromString "gamma") g)
              greeks `shouldSatisfy` \g -> isJust (KeyMap.lookup (Key.fromString "vega") g)
              greeks `shouldSatisfy` \g -> isJust (KeyMap.lookup (Key.fromString "theta") g)
              greeks `shouldSatisfy` \g -> isJust (KeyMap.lookup (Key.fromString "rho") g)
            _ -> expectationFailure "Greeks field missing or not an object"
        Just _ -> expectationFailure "Response is not a JSON object"
