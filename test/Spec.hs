{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import qualified Data.Aeson          as Aeson
import           Hedgehog
import           Hedgehog.Gen.JSON
import qualified Hedgehog.Range      as Range
import           Protolude
import           Test.Tasty
import           Test.Tasty.Hedgehog

ranges :: Ranges
ranges =
  Ranges
  { _arrayRange = ArrayRange {unArrayRange = (Range.linear 0 5)}
  , _stringRange = StringRange {unStringRange = (Range.linear 0 100)}
  , _numberRange = NumberRange {unNumberRange = (Range.linearFrac 0 1000)}
  , _objectRange = ObjectRange {unObjectRange = (Range.linear 0 5)}
  }

prop_generatedUnconstrainedJSON :: Property
prop_generatedUnconstrainedJSON =
  property $ do
    v <- forAll $ genJSON ranges
    assert $ isJust ((Aeson.decodeStrict v) :: Maybe Aeson.Value)

tests :: TestTree
tests =
  testGroup
    "Hedgehog.Gen.JSON tests"
    [testProperty "Generated unconstrained values are valid JSON" prop_generatedUnconstrainedJSON]

main :: IO ()
main = defaultMain tests
