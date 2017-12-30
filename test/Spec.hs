{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens                 (over, set)
import qualified Data.Aeson                   as Aeson
import           Data.Fixed                   (mod')
import qualified Data.HashMap.Strict          as H
import qualified Data.Scientific              as Scientific
import qualified Data.Text                    as Text
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import           Hedgehog.Gen.JSON
import           Hedgehog.Gen.JSON.JSONSchema
import qualified Hedgehog.Range               as Range
import qualified Prelude                      as P
import           Protolude
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Text.Regex.Posix

ranges :: Ranges
ranges =
  Ranges
  { _arrayRange = ArrayRange $ Range.linear 0 5
  , _stringRange = StringRange $ Range.linear 0 100
  , _numberRange = NumberRange $ Range.linearFrac 0 1000
  , _objectRange = ObjectRange $ Range.linear 0 5
  }

prop_generatedUnconstrainedJSON :: Property
prop_generatedUnconstrainedJSON =
  property $ do
    v <- forAll $ genJSON ranges
    assert $ isJust (Aeson.decodeStrict v :: Maybe Aeson.Value)

prop_constrainedValueFromEnum :: Property
prop_constrainedValueFromEnum =
  property $ do
    values <- forAll $ Gen.nonEmpty (Range.linear 1 10) (Gen.text (Range.linear 1 100) Gen.unicode)
    schema <- forAll $ over schemaEnum (const $ Just $ AnyConstraintEnum (Aeson.String <$> values)) <$> genSchema
    v <- forAll $ genConstrainedJSON ranges schema
    assert $ isJust $ find (== Aeson.decodeStrict v) (Just <$> values)

prop_constrainedValueFromConst :: Property
prop_constrainedValueFromConst =
  property $ do
    c <- forAll $ genJSONValue ranges
    schema <- forAll $ over schemaConst (const $ Just $ AnyConstraintConst c) <$> genSchema
    v <- forAll $ genConstrainedJSON ranges schema
    Aeson.decodeStrict v === Just c

prop_constrainedNumber :: Property
prop_constrainedNumber =
  property $ do
    vmin <- forAll $ Gen.maybe $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 0 5000)
    vminEx <- forAll $ Gen.maybe $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 0 5000)
    vmax <- forAll $ Gen.maybe $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 6000 10000)
    vmaxEx <- forAll $ Gen.maybe $ Scientific.fromFloatDigits <$> Gen.double (Range.linearFrac 6000 10000)
    let schema =
          (set schemaMinimum (NumberConstraintMinimum <$> vmin) .
           set schemaMaximum (NumberConstraintMaximum <$> vmax) .
           set schemaExclusiveMinimum (NumberConstraintExclusiveMinimum <$> vminEx) .
           set schemaExclusiveMaximum (NumberConstraintExclusiveMaximum <$> vmaxEx))
            numberSchema
    (Aeson.Number v) <- forAll $ genConstrainedJSONValue ranges schema
    assert $ maybe True (v >=) vmin
    assert $ maybe True (v <=) vmax
    assert $ maybe True (v <) vmaxEx
    assert $ maybe True (v >) vminEx

prop_constrainedInteger :: Property
prop_constrainedInteger =
  property $ do
    vmin <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 0 5000)
    vminEx <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 0 5000)
    vmax <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 6000 10000)
    vmaxEx <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 6000 10000)
    multipleOf <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 1 10)
    let schema =
          (set schemaMinimum (NumberConstraintMinimum <$> vmin) .
           set schemaMaximum (NumberConstraintMaximum <$> vmax) .
           set schemaExclusiveMinimum (NumberConstraintExclusiveMinimum <$> vminEx) .
           set schemaExclusiveMaximum (NumberConstraintExclusiveMaximum <$> vmaxEx) .
           set schemaMultipleOf (NumberConstraintMultipleOf <$> multipleOf))
            integerSchema
    (Aeson.Number v) <- forAll $ genConstrainedJSONValue ranges schema
    assert $ maybe True (v >=) vmin
    assert $ maybe True (v <=) vmax
    assert $ maybe True (v <) vmaxEx
    assert $ maybe True (v >) vminEx
    assert $ maybe True (\x -> v `mod'` x == 0) multipleOf
    assert $ Scientific.isInteger v

prop_constrainedString :: Property
prop_constrainedString =
  property $ do
    minLength <- forAll $ Gen.maybe $ Gen.integral (Range.linear 0 500)
    maxLength <- forAll $ Gen.maybe $ Gen.integral (Range.linear (fromMaybe 0 minLength) 1000)
    regexp <- forAll $ Gen.maybe $ Gen.constant "[a-zA-Z0-9]{3,9}" -- Not very arbitrary, I know.
    let schema =
          (set schemaPattern (StringConstraintPattern <$> regexp) .
           set schemaMinLength (StringConstraintMinLength <$> minLength) .
           set schemaMaxLength (StringConstraintMaxLength <$> maxLength))
            stringSchema
    (Aeson.String v) <- forAll $ genConstrainedJSONValue ranges schema
    assert $
      case regexp of
        Just p -> Text.unpack v =~ Text.unpack p
        Nothing -> Text.length v >= fromMaybe 0 minLength && Text.length v <= fromMaybe 1000 maxLength

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
          (Just . ObjectConstraintProperties) (H.fromList [("user_id", integerSchema), ("user_domain", stringSchema)])
      , _schemaPattern = Nothing
      , _schemaMaxLength = Nothing
      , _schemaMinLength = Nothing
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
    , testProperty "Generates a constrained integer" prop_constrainedInteger
    , testProperty "Generates a constrained string" prop_constrainedString
    ]

main :: IO ()
main = defaultMain tests

genSchema :: Gen Schema
genSchema =
  Gen.element [nullSchema, booleanSchema, objectSchema, arraySchema, numberSchema, integerSchema, stringSchema]
