-- |
-- Module: OptionSpec
-- Description: Test suite for Option module
--
-- This test suite validates:
-- - Smart constructor validation (TimeToExpiryDays)
-- - JSON serialization and deserialization
-- - Getter functions
-- - Error cases and edge conditions
module OptionSpec (spec) where

import Data.Aeson qualified as Aeson
import Option qualified
import Protolude
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (..))


spec :: Spec
spec = do
  describe "TimeToExpiryDays - Smart Constructor" $ do
    it "accepts positive values" $ do
      case Option.mkTimeToExpiryDays 1 of
        Right expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 1
        Left err -> expectationFailure $ "Unexpected error: " <> show err
      case Option.mkTimeToExpiryDays 7 of
        Right expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 7
        Left err -> expectationFailure $ "Unexpected error: " <> show err
      case Option.mkTimeToExpiryDays 30 of
        Right expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 30
        Left err -> expectationFailure $ "Unexpected error: " <> show err
      case Option.mkTimeToExpiryDays 365 of
        Right expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 365
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "accepts very small positive values" $ do
      case Option.mkTimeToExpiryDays 0.001 of
        Right expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 0.001
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "rejects zero" $ do
      Option.mkTimeToExpiryDays 0 `shouldBe` Left Option.TimeToExpiryNotPositive

    it "rejects negative values" $ do
      Option.mkTimeToExpiryDays (-1) `shouldBe` Left Option.TimeToExpiryNotPositive
      Option.mkTimeToExpiryDays (-100) `shouldBe` Left Option.TimeToExpiryNotPositive

    it "rejects NaN" $ do
      Option.mkTimeToExpiryDays (0 / 0) `shouldBe` Left Option.TimeToExpiryNotFinite

    it "rejects positive infinity" $ do
      Option.mkTimeToExpiryDays (1 / 0) `shouldBe` Left Option.TimeToExpiryNotFinite

    it "rejects negative infinity" $ do
      Option.mkTimeToExpiryDays ((-1) / 0)
        `shouldBe` Left Option.TimeToExpiryNotFinite

  describe "TimeToExpiryDays - Getter" $ do
    it "extracts the days value" $ do
      case Option.mkTimeToExpiryDays 30 of
        Right expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 30
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "round-trips the value" $ do
      case Option.mkTimeToExpiryDays 42.5 of
        Right expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 42.5
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "TimeToExpiryDays - JSON Serialization" $ do
    it "serializes to an object with days field" $ do
      case Option.mkTimeToExpiryDays 30 of
        Right expiry -> Aeson.encode expiry `shouldBe` "{\"days\":30}"
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "deserializes valid positive number" $ do
      let json = "{\"days\":30}"
          result = Aeson.decode @Option.TimeToExpiryDays json
      case result of
        Just expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 30
        Nothing -> expectationFailure "Failed to deserialize valid JSON"

    it "deserializes fractional values" $ do
      let json = "{\"days\":7.5}"
          result = Aeson.decode @Option.TimeToExpiryDays json
      case result of
        Just expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 7.5
        Nothing -> expectationFailure "Failed to deserialize valid JSON"

    it "rejects zero in JSON" $ do
      let json = "{\"days\":0}"
          result = Aeson.decode @Option.TimeToExpiryDays json
      result `shouldBe` Nothing

    it "rejects negative values in JSON" $ do
      let json = "{\"days\":-5}"
          result = Aeson.decode @Option.TimeToExpiryDays json
      result `shouldBe` Nothing

    it "round-trips through JSON" $ do
      case Option.mkTimeToExpiryDays 90 of
        Right original ->
          let encoded = Aeson.encode original
              decoded = Aeson.decode @Option.TimeToExpiryDays encoded
           in case decoded of
                Just expiry -> Option.getTimeToExpiryDays expiry `shouldBe` 90
                Nothing -> expectationFailure "Failed to round-trip through JSON"
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "TimeToExpiryDays - Property Tests" $ do
    prop "all positive values are accepted" $
      \(Positive days) ->
        case Option.mkTimeToExpiryDays days of
          Right expiry -> Option.getTimeToExpiryDays expiry == days
          Left _ -> False

    prop "all negative values are rejected" $
      \(Positive days) ->
        let negative = -days
         in Option.mkTimeToExpiryDays negative == Left Option.TimeToExpiryNotPositive

    prop "round-trips through smart constructor" $
      \(Positive days) ->
        case Option.mkTimeToExpiryDays days of
          Right expiry ->
            case Option.mkTimeToExpiryDays (Option.getTimeToExpiryDays expiry) of
              Right expiry2 -> Option.getTimeToExpiryDays expiry == Option.getTimeToExpiryDays expiry2
              Left _ -> False
          Left _ -> False

    prop "round-trips through JSON serialization" $
      \(Positive days) ->
        case Option.mkTimeToExpiryDays days of
          Right original ->
            let encoded = Aeson.encode original
                decoded = Aeson.decode @Option.TimeToExpiryDays encoded
             in case decoded of
                  Just expiry -> Option.getTimeToExpiryDays expiry == days
                  Nothing -> False
          Left _ -> False

  describe "OptionKind - JSON Serialization" $ do
    it "serializes Call correctly" $ do
      Aeson.encode Option.Call `shouldBe` "\"Call\""

    it "serializes Put correctly" $ do
      Aeson.encode Option.Put `shouldBe` "\"Put\""

    it "deserializes Call correctly" $ do
      Aeson.decode @Option.OptionKind "\"Call\"" `shouldBe` Just Option.Call

    it "deserializes Put correctly" $ do
      Aeson.decode @Option.OptionKind "\"Put\"" `shouldBe` Just Option.Put

    it "rejects invalid option kinds" $ do
      Aeson.decode @Option.OptionKind "\"Invalid\"" `shouldBe` Nothing

  describe "TimeToExpiryError - JSON Serialization" $ do
    it "serializes TimeToExpiryNotFinite" $ do
      Aeson.encode Option.TimeToExpiryNotFinite `shouldBe` "\"TimeToExpiryNotFinite\""

    it "serializes TimeToExpiryNotPositive" $ do
      Aeson.encode Option.TimeToExpiryNotPositive
        `shouldBe` "\"TimeToExpiryNotPositive\""

    it "deserializes TimeToExpiryNotFinite" $ do
      Aeson.decode @Option.TimeToExpiryError "\"TimeToExpiryNotFinite\""
        `shouldBe` Just Option.TimeToExpiryNotFinite

    it "deserializes TimeToExpiryNotPositive" $ do
      Aeson.decode @Option.TimeToExpiryError "\"TimeToExpiryNotPositive\""
        `shouldBe` Just Option.TimeToExpiryNotPositive
