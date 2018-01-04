{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Gen.JSON.Constrained.Internal.InternalSpec
  ( tests
  ) where

import           Control.Lens                           (over, set)
import qualified Data.Aeson                             as Aeson
import           Data.Fixed                             (mod')
import qualified Data.HashMap.Strict                    as H
import qualified Data.HashSet                           as HashSet
import qualified Data.Scientific                        as Scientific
import qualified Data.Text                              as Text
import           Hedgehog
import qualified Hedgehog.Gen                           as Gen
import           Hedgehog.Gen.JSON
import           Hedgehog.Gen.JSON
import           Hedgehog.Gen.JSON.Constrained.Internal
import           Hedgehog.Gen.JSON.JSONSchema
import qualified Hedgehog.Range                         as Range
import qualified Prelude                                as P
import           Protolude
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Text.Regex.Posix
import           Text.Regex.Posix

prop_genBoundedInteger :: Property
prop_genBoundedInteger =
  property $ do
    minC <- forAll $ Gen.maybe $ Gen.integral (Range.linear (-500) 500)
    maxC <- forAll $ Gen.maybe $ Gen.integral (Range.linear 500 1500)
    minExC <- forAll $ Gen.maybe $ Gen.integral (Range.linear (-500) 500)
    maxExC <- forAll $ Gen.maybe $ Gen.integral (Range.linear 500 1500)
    multC <- forAll $ Gen.maybe $ Gen.integral (Range.linear 1 10)
    v <-
      forAll $
      genBoundedInteger
        ((NumberConstraintExclusiveMinimum . fromInteger) <$> minExC)
        ((NumberConstraintMinimum . fromInteger) <$> minC)
        ((NumberConstraintExclusiveMaximum . fromInteger) <$> maxExC)
        ((NumberConstraintMaximum . fromInteger) <$> maxC)
        ((NumberConstraintMultipleOf . fromInteger) <$> multC)
        (Range.linear (-5000) 5000)
    assert $ maybe True (v >=) minC
    assert $ maybe True (v <=) maxC
    assert $ maybe True (v >) minExC
    assert $ maybe True (v <) maxExC
    assert $ maybe True (\m -> v `rem` m == 0) multC

prop_genBoundedReal :: Property
prop_genBoundedReal =
  property $ do
    minC <- forAll $ Gen.maybe $ Gen.double (Range.linearFrac (-500) 500)
    maxC <- forAll $ Gen.maybe $ Gen.double (Range.linearFrac 500 1500)
    minExC <- forAll $ Gen.maybe $ Gen.double (Range.linearFrac (-500) 500)
    maxExC <- forAll $ Gen.maybe $ Gen.double (Range.linearFrac 500 1500)
    v <-
      forAll $
      genBoundedReal
        ((NumberConstraintExclusiveMinimum . Scientific.fromFloatDigits) <$> minExC)
        ((NumberConstraintMinimum . Scientific.fromFloatDigits) <$> minC)
        ((NumberConstraintExclusiveMaximum . Scientific.fromFloatDigits) <$> maxExC)
        ((NumberConstraintMaximum . Scientific.fromFloatDigits) <$> maxC)
        (Range.linearFrac (-5000) 5000)
    assert $ maybe True (v >=) minC
    assert $ maybe True (v <=) maxC
    assert $ maybe True (v >) minExC
    assert $ maybe True (v <) maxExC

prop_genBoundedString :: Property
prop_genBoundedString =
  property $ do
    minLengthC <- forAll $ Gen.maybe $ Gen.int (Range.linear 0 100)
    maxLengthC <- forAll $ Gen.maybe $ Gen.int (Range.linear 100 200)
    v <-
      forAll $
      genBoundedString
        ((StringConstraintMinLength . fromIntegral) <$> minLengthC)
        ((StringConstraintMaxLength . fromIntegral) <$> maxLengthC)
        (Range.linear 0 500)
    assert $ maybe True (\n -> Text.length v >= n) minLengthC
    assert $ maybe True (\n -> Text.length v <= n) maxLengthC

prop_genStringFromRegexp :: Property
prop_genStringFromRegexp =
  property $ do
    let regexp = "[a-zA-Z0-9]{3,9}"
    v <- forAll $ genStringFromRegexp regexp
    assert $ Text.unpack v =~ Text.unpack regexp

prop_genUniqueItems :: Property
prop_genUniqueItems = property $ do
  let gen = Gen.int (Range.linear 0 1000)
  generated <- forAll $ genUniqueItems (Range.linear 10 50) gen
  assert $ length generated <= 50
  assert $ length generated >= 10
  (length . HashSet.fromList) generated === length generated

tests :: TestTree
tests =
  testGroup
    "Constrained.Internal"
    [ testProperty "Generates a bounded integer" prop_genBoundedInteger
    , testProperty "Generates a bounded real" prop_genBoundedReal
    , testProperty "Generates a bounded string" prop_genBoundedString
    , testProperty "Generates a string from a regexp" prop_genStringFromRegexp
    , testProperty "Generates a list of unique items" prop_genUniqueItems
    ]
