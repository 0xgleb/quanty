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
import Option
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (..))


spec :: Spec
spec = do
  describe "TimeToExpiryDays - Smart Constructor" $ do
    it "accepts positive values" $ do
      case mkTimeToExpiryDays 1 of
        Right t -> getTimeToExpiryDays t `shouldBe` 1
        Left err -> expectationFailure $ "Unexpected error: " <> show err
      case mkTimeToExpiryDays 7 of
        Right t -> getTimeToExpiryDays t `shouldBe` 7
        Left err -> expectationFailure $ "Unexpected error: " <> show err
      case mkTimeToExpiryDays 30 of
        Right t -> getTimeToExpiryDays t `shouldBe` 30
        Left err -> expectationFailure $ "Unexpected error: " <> show err
      case mkTimeToExpiryDays 365 of
        Right t -> getTimeToExpiryDays t `shouldBe` 365
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "accepts very small positive values" $ do
      case mkTimeToExpiryDays 0.001 of
        Right t -> getTimeToExpiryDays t `shouldBe` 0.001
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "rejects zero" $ do
      mkTimeToExpiryDays 0 `shouldBe` Left TimeToExpiryNotPositive

    it "rejects negative values" $ do
      mkTimeToExpiryDays (-1) `shouldBe` Left TimeToExpiryNotPositive
      mkTimeToExpiryDays (-100) `shouldBe` Left TimeToExpiryNotPositive

    it "rejects NaN" $ do
      mkTimeToExpiryDays (0 / 0) `shouldBe` Left TimeToExpiryNotFinite

    it "rejects positive infinity" $ do
      mkTimeToExpiryDays (1 / 0) `shouldBe` Left TimeToExpiryNotFinite

    it "rejects negative infinity" $ do
      mkTimeToExpiryDays ((-1) / 0) `shouldBe` Left TimeToExpiryNotFinite

  describe "TimeToExpiryDays - Getter" $ do
    it "extracts the days value" $ do
      case mkTimeToExpiryDays 30 of
        Right t -> getTimeToExpiryDays t `shouldBe` 30
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "round-trips the value" $ do
      case mkTimeToExpiryDays 42.5 of
        Right t -> getTimeToExpiryDays t `shouldBe` 42.5
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "TimeToExpiryDays - JSON Serialization" $ do
    it "serializes to a number" $ do
      case mkTimeToExpiryDays 30 of
        Right t -> Aeson.encode t `shouldBe` "30"
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "deserializes valid positive number" $ do
      let json = "30"
          result = Aeson.decode json :: Maybe TimeToExpiryDays
      case result of
        Just t -> getTimeToExpiryDays t `shouldBe` 30
        Nothing -> expectationFailure "Failed to deserialize valid JSON"

    it "deserializes fractional values" $ do
      let json = "7.5"
          result = Aeson.decode json :: Maybe TimeToExpiryDays
      case result of
        Just t -> getTimeToExpiryDays t `shouldBe` 7.5
        Nothing -> expectationFailure "Failed to deserialize valid JSON"

    it "rejects zero in JSON" $ do
      let json = "0"
          result = Aeson.decode json :: Maybe TimeToExpiryDays
      result `shouldBe` Nothing

    it "rejects negative values in JSON" $ do
      let json = "-5"
          result = Aeson.decode json :: Maybe TimeToExpiryDays
      result `shouldBe` Nothing

    it "round-trips through JSON" $ do
      case mkTimeToExpiryDays 90 of
        Right original ->
          let encoded = Aeson.encode original
              decoded = Aeson.decode encoded :: Maybe TimeToExpiryDays
           in case decoded of
                Just t -> getTimeToExpiryDays t `shouldBe` 90
                Nothing -> expectationFailure "Failed to round-trip through JSON"
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "TimeToExpiryDays - Property Tests" $ do
    prop "all positive values are accepted" $
      \(Positive days) ->
        case mkTimeToExpiryDays days of
          Right t -> getTimeToExpiryDays t == days
          Left _ -> False

    prop "all negative values are rejected" $
      \(Positive days) ->
        let negative = -days
         in mkTimeToExpiryDays negative == Left TimeToExpiryNotPositive

    prop "round-trips through smart constructor" $
      \(Positive days) ->
        case mkTimeToExpiryDays days of
          Right t ->
            case mkTimeToExpiryDays (getTimeToExpiryDays t) of
              Right t2 -> getTimeToExpiryDays t == getTimeToExpiryDays t2
              Left _ -> False
          Left _ -> False

    prop "round-trips through JSON serialization" $
      \(Positive days) ->
        case mkTimeToExpiryDays days of
          Right original ->
            let encoded = Aeson.encode original
                decoded = Aeson.decode encoded :: Maybe TimeToExpiryDays
             in case decoded of
                  Just t -> getTimeToExpiryDays t == days
                  Nothing -> False
          Left _ -> False

  describe "OptionKind - JSON Serialization" $ do
    it "serializes Call correctly" $ do
      Aeson.encode Call `shouldBe` "\"Call\""

    it "serializes Put correctly" $ do
      Aeson.encode Put `shouldBe` "\"Put\""

    it "deserializes Call correctly" $ do
      Aeson.decode "\"Call\"" `shouldBe` Just Call

    it "deserializes Put correctly" $ do
      Aeson.decode "\"Put\"" `shouldBe` Just Put

    it "rejects invalid option kinds" $ do
      (Aeson.decode "\"Invalid\"" :: Maybe OptionKind) `shouldBe` Nothing

  describe "TimeToExpiryError - JSON Serialization" $ do
    it "serializes TimeToExpiryNotFinite" $ do
      Aeson.encode TimeToExpiryNotFinite `shouldBe` "\"TimeToExpiryNotFinite\""

    it "serializes TimeToExpiryNotPositive" $ do
      Aeson.encode TimeToExpiryNotPositive `shouldBe` "\"TimeToExpiryNotPositive\""

    it "deserializes TimeToExpiryNotFinite" $ do
      Aeson.decode "\"TimeToExpiryNotFinite\"" `shouldBe` Just TimeToExpiryNotFinite

    it "deserializes TimeToExpiryNotPositive" $ do
      Aeson.decode "\"TimeToExpiryNotPositive\""
        `shouldBe` Just TimeToExpiryNotPositive
