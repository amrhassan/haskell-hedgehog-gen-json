{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens                 (over, set)
import qualified Data.Aeson                   as Aeson
import qualified Data.HashMap.Strict          as H
import qualified Data.Scientific              as Scientific
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import           Hedgehog.Gen.JSON
import           Hedgehog.Gen.JSON.JSONSchema
import qualified Hedgehog.Range               as Range
import qualified Prelude                      as P
import           Protolude
import           Test.Tasty
import           Test.Tasty.Hedgehog

ranges :: Ranges
ranges =
  Ranges
  { _arrayRange = ArrayRange {unArrayRange = Range.linear 0 5}
  , _stringRange = StringRange {unStringRange = Range.linear 0 100}
  , _numberRange = NumberRange {unNumberRange = Range.linearFrac 0 1000}
  , _objectRange = ObjectRange {unObjectRange = Range.linear 0 5}
  }

--genTypes :: Gen AnyKeywordType
--genTypes = Gen.choice [SingleType <$> Gen.enumBounded, MultipleTypes <$> Gen.set (Range.linear 1 10) Gen.enumBounded]
prop_generatedUnconstrainedJSON :: Property
prop_generatedUnconstrainedJSON =
  property $ do
    v <- forAll $ genJSON ranges
    assert $ isJust (Aeson.decodeStrict v :: Maybe Aeson.Value)

prop_constrainedValueFromEnum :: Property
prop_constrainedValueFromEnum =
  property $ do
    values <- forAll $ Gen.nonEmpty (Range.linear 1 10) (Gen.text (Range.linear 1 100) Gen.unicode)
    schema <- forAll $ over schemaEnum (const $ Just $ AnyKeywordEnum (Aeson.String <$> values)) <$> genSchema
    v <- forAll $ genConstrainedJSON ranges schema
    assert $ isJust $ find (== Aeson.decodeStrict v) (Just <$> values)

prop_constrainedValueFromConst :: Property
prop_constrainedValueFromConst =
  property $ do
    c <- forAll $ genJSONValue ranges
    schema <- forAll $ over schemaConst (const $ Just $ AnyKeywordConst c) <$> genSchema
    v <- forAll $ genConstrainedJSON ranges schema
    Aeson.decodeStrict v === Just c

prop_constrainedNumber :: Property
prop_constrainedNumber =
  property $ do
    vmin <- forAll $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 0 5000)
    vminEx <- forAll $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 0 5000)
    vmax <- forAll $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 6000 10000)
    vmaxEx <- forAll $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 6000 10000)
    let schema =
          (set schemaMinimum (Just $ NumberKeywordMinimum vmin) .
           set schemaMaximum (Just $ NumberKeywordMaximum vmax) .
           set schemaExclusiveMinimum (Just $ NumberKeywordExclusiveMinimum vminEx) .
           set schemaExclusiveMaximum (Just $ NumberKeywordExclusiveMaximum vmaxEx))
            numberSchema
    (Aeson.Number v) <- forAll $ genConstrainedJSONValue ranges schema
    assert (v >= vmin && v > vminEx && v <= vmax && v < vmaxEx)

prop_decodesSchema :: Property
prop_decodesSchema = property $ decoded === Right expected
  where
    schemaJson =
      "{\"type\":\"object\",\"properties\":{\"user_id\":{\"type\":\"integer\"},\"user_domain\":{\"type\":\"string\"}},\"required\":[\"user_id\"]}"
    decoded :: Either P.String Schema = Aeson.eitherDecode schemaJson
    expected =
      Schema
      { _schemaType = SingleType ObjectType
      , _schemaEnum = Nothing
      , _schemaConst = Nothing
      , _schemaRequired = Just (ObjectKeywordRequired ["user_id"])
      , _schemaMultipleOf = Nothing
      , _schemaMaximum = Nothing
      , _schemaMinimum = Nothing
      , _schemaExclusiveMaximum = Nothing
      , _schemaExclusiveMinimum = Nothing
      , _schemaProperties =
          (Just . ObjectKeywordProperties) (H.fromList [("user_id", integerSchema), ("user_domain", stringSchema)])
      }

tests :: TestTree
tests =
  testGroup
    "Hedgehog.Gen.JSON tests"
    [ testProperty "Generated unconstrained values are valid JSON" prop_generatedUnconstrainedJSON
    , testProperty "Decodes JSON Schema" prop_decodesSchema
    , testProperty "Generates constrained values from JSON Schema enum when present" prop_constrainedValueFromEnum
    , testProperty "Generates constrained values from JSON Schema const when present" prop_constrainedValueFromConst
    , testProperty "Generates a constrained number" prop_constrainedNumber
    ]

main :: IO ()
main = defaultMain tests

genSchema :: Gen Schema
genSchema =
  Gen.element [nullSchema, booleanSchema, objectSchema, arraySchema, numberSchema, integerSchema, stringSchema]
