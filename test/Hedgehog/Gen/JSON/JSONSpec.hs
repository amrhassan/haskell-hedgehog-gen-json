{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Gen.JSON.JSONSpec
  ( tests
  ) where

import           Control.Lens                 (over, set)
import qualified Data.Aeson                   as Aeson
import           Data.Fixed                   (mod')
import qualified Data.HashMap.Strict          as H
import qualified Data.HashSet                 as HS
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

prop_generatedUnconstrainedJSON :: Property
prop_generatedUnconstrainedJSON =
  property $ do
    v <- forAll $ genJSON sensibleRanges
    assert $ isJust (Aeson.decodeStrict v :: Maybe Aeson.Value)

prop_constrainedValueFromEnum :: Property
prop_constrainedValueFromEnum =
  property $ do
    values <- forAll $ Gen.nonEmpty (Range.linear 1 10) (Gen.text (Range.linear 1 100) Gen.unicode)
    schema <- forAll $ over schemaEnum (const $ Just $ AnyConstraintEnum (Aeson.String <$> values)) <$> genSchema
    v <- forAll $ genConstrainedJSON sensibleRanges schema
    assert $ isJust $ find (== Aeson.decodeStrict v) (Just <$> values)

prop_constrainedValueFromConst :: Property
prop_constrainedValueFromConst =
  property $ do
    c <- forAll $ genJSONValue sensibleRanges
    schema <- forAll $ over schemaConst (const $ Just $ AnyConstraintConst c) <$> genSchema
    v <- forAll $ genConstrainedJSON sensibleRanges schema
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
            (singleTypeSchema NumberType)
    (Aeson.Number v) <- forAll $ genConstrainedJSONValue sensibleRanges schema
    assert $ maybe True (v >=) vmin
    assert $ maybe True (v <=) vmax
    assert $ maybe True (v <) vmaxEx
    assert $ maybe True (v >) vminEx

prop_constrainedInteger :: Property
prop_constrainedInteger =
  property $ do
    vmin <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear (-50) 50)
    vminEx <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear (-50) 50)
    vmax <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 50 100)
    vmaxEx <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 50 100)
    multipleOf <- forAll $ Gen.maybe $ fromInteger <$> Gen.integral (Range.linear 1 10)
    let schema =
          (set schemaMinimum (NumberConstraintMinimum <$> vmin) .
           set schemaMaximum (NumberConstraintMaximum <$> vmax) .
           set schemaExclusiveMinimum (NumberConstraintExclusiveMinimum <$> vminEx) .
           set schemaExclusiveMaximum (NumberConstraintExclusiveMaximum <$> vmaxEx) .
           set schemaMultipleOf (NumberConstraintMultipleOf <$> multipleOf))
            (singleTypeSchema IntegerType)
    (Aeson.Number v) <- forAll $ genConstrainedJSONValue sensibleRanges schema
    assert $ maybe True (v >=) vmin
    assert $ maybe True (v <=) vmax
    assert $ maybe True (v <) vmaxEx
    assert $ maybe True (v >) vminEx
    assert $ maybe True (\x -> v `mod'` x == 0) multipleOf
    assert $ Scientific.isInteger v

prop_constrainedString :: Property
prop_constrainedString =
  property $ do
    minLength <- forAll $ Gen.maybe $ Gen.integral (Range.linear 0 50)
    maxLength <- forAll $ Gen.maybe $ Gen.integral (Range.linear 50 100)
    regexp <- forAll $ Gen.maybe $ Gen.constant "[a-zA-Z0-9]{3,9}" -- Not very arbitrary, I know.
    let schema =
          (set schemaPattern (StringConstraintPattern <$> regexp) .
           set schemaMinLength (StringConstraintMinLength <$> minLength) .
           set schemaMaxLength (StringConstraintMaxLength <$> maxLength))
            (singleTypeSchema StringType)
    (Aeson.String v) <- forAll $ genConstrainedJSONValue sensibleRanges schema
    assert $
      case regexp of
        Just p -> Text.unpack v =~ Text.unpack p
        Nothing -> Text.length v >= fromMaybe 0 minLength && Text.length v <= fromMaybe 1000 maxLength

prop_constrainedObject :: Property
prop_constrainedObject =
  property $ do
    nonRequiredFields <-
      forAll $
      H.fromList <$> (Gen.list (Range.linear 1 10) $ ((,) <$> (Gen.text (Range.linear 1 10) Gen.unicode) <*> genSchema))
    requiredFields <-
      forAll $
      H.fromList <$> (Gen.list (Range.linear 1 10) $ ((,) <$> (Gen.text (Range.linear 1 10) Gen.unicode) <*> genSchema))
    let schema =
          (set schemaProperties (Just $ ObjectConstraintProperties (nonRequiredFields `H.union` requiredFields)) .
           set schemaRequired (Just $ ObjectConstraintRequired $ H.keys requiredFields))
            (singleTypeSchema ObjectType)
    (Aeson.Object v) <- forAll $ genConstrainedJSONValue sensibleRanges schema
    assert $ all (`elem` (H.keys v)) (H.keys requiredFields)

prop_constrainedArray :: Property
prop_constrainedArray =
  property $ do
    uniqueItems <- forAll $ Gen.maybe Gen.bool
    minItems <- forAll $ Gen.maybe $ Gen.integral (Range.linear 0 2)
    maxItems <- forAll $ Gen.maybe $ Gen.integral (Range.linear 3 5)
    itemSchema <- forAll genSchema
    let schema =
          (set schemaMinItems (ArrayConstraintMinItems <$> minItems) .
           set schemaMaxItems (ArrayConstraintMaxItems <$> maxItems) .
           set schemaUniqueItems (ArrayConstraintUniqueItems <$> uniqueItems) .
           set schemaItems (Just $ ArrayConstraintItems itemSchema))
            (singleTypeSchema ArrayType)
    (Aeson.Array v) <- forAll $ genConstrainedJSONValue sensibleRanges schema
    assert $ maybe True (length v >=) minItems
    assert $ maybe True (length v <=) maxItems
    assert $
      maybe
        True
        (\b ->
           if b
             then isUnique v
             else True)
        uniqueItems

prop_decodesSchema :: Property
prop_decodesSchema = property $ decoded === Right expected
  where
    schemaJson =
      "{\"type\":\"object\",\"properties\":{\"user_id\":{\"type\":\"integer\"},\"user_domain\":{\"type\":\"string\"}},\"required\":[\"user_id\"]}"
    decoded :: Either P.String Schema = Aeson.eitherDecode schemaJson
    expected =
      (set schemaRequired (Just (ObjectConstraintRequired ["user_id"])) .
       set
         schemaProperties
         ((Just . ObjectConstraintProperties)
            (H.fromList [("user_id", singleTypeSchema IntegerType), ("user_domain", singleTypeSchema StringType)])))
        (singleTypeSchema ObjectType)

genSchema :: Gen Schema
genSchema = do
  t <- Gen.enumBounded
  pure $ singleTypeSchema t

singleTypeSchema :: PrimitiveType -> Schema
singleTypeSchema t = set schemaType (Just $ SingleType t) emptySchema

isUnique :: (Hashable a, Eq a, Foldable t) => t a -> Bool
isUnique xs = (toList . HS.fromList . toList) xs == toList xs

tests :: TestTree
tests =
  testGroup
    "Generated JSON"
    [ testProperty "Generated unconstrained values are valid JSON" prop_generatedUnconstrainedJSON
    , testProperty "Decodes JSON Schema" prop_decodesSchema
    , testProperty "Generates constrained values from JSON Schema enum when present" prop_constrainedValueFromEnum
    , testProperty "Generates constrained values from JSON Schema const when present" prop_constrainedValueFromConst
    , testProperty "Generates a constrained number" prop_constrainedNumber
    , testProperty "Generates a constrained integer" prop_constrainedInteger
    , testProperty "Generates a constrained string" prop_constrainedString
    , testProperty "Generates a constrained object" prop_constrainedObject
    , testProperty "Generates a constrained array" prop_constrainedArray
    ]
