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
spec = Wai.with (pure Api.app) $ do
  describe "GET /api/v1/health" $ do
    it "responds with 200" $ do
      Wai.get "/api/v1/health" `Hspec.shouldRespondWith` 200

    it "returns valid health status" $ do
      let expected = [Hspec.json|{status: "ok", version: "0.1.0"}|] {matchStatus = 200}
      Hspec.get "/api/v1/health" `Hspec.shouldRespondWith` expected

    it "returns JSON with status and version fields" $ do
      response <- Hspec.get "/api/v1/health"
      let maybeJson = Aeson.decode @Aeson.Value (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid JSON"
        Just (Aeson.Object obj) -> do
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "status") o)
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "version") o)
        Just _ -> expectationFailure "Response is not a JSON object"

  describe "GET /api/v1/placeholder" $ do
    it "responds with 200" $ do
      Hspec.get "/api/v1/placeholder" `Hspec.shouldRespondWith` 200

    it "returns JSON with message and timestamp fields" $ do
      response <- Hspec.get "/api/v1/placeholder"
      let maybeJson = Aeson.decode @Aeson.Value (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid JSON"
        Just (Aeson.Object obj) -> do
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "message") o)
          obj `shouldSatisfy` \o -> isJust (KeyMap.lookup (Key.fromString "timestamp") o)
        Just _ -> expectationFailure "Response is not a JSON object"

    it "returns expected message text" $ do
      response <- Hspec.get "/api/v1/placeholder"
      let maybeJson = Aeson.decode @Api.PlaceholderResponse (Wai.simpleBody response)
      liftIO $ case maybeJson of
        Nothing -> expectationFailure "Response is not valid PlaceholderResponse"
        Just placeholderResp ->
          placeholderResp.message `shouldBe` "Hello from Quanty API"
