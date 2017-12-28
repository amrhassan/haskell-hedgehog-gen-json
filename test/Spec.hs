{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens                 ((^.))
import qualified Data.Aeson                   as Aeson
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as H
import qualified Data.Set                     as S
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

genTypes :: Gen AnyKeywordType
genTypes = Gen.choice [SingleType <$> Gen.enumBounded, MultipleTypes <$> Gen.set (Range.linear 1 10) Gen.enumBounded]

prop_generatedUnconstrainedJSON :: Property
prop_generatedUnconstrainedJSON =
  property $ do
    v <- forAll $ genJSON ranges
    assert $ isJust (Aeson.decodeStrict v :: Maybe Aeson.Value)

prop_constrainedValueFromEnum :: Property
prop_constrainedValueFromEnum =
  property $ do
    st <- forAll genTypes
    values <- forAll $ Gen.nonEmpty (Range.linear 1 10) (Gen.text (Range.linear 1 100) Gen.unicode)
    let schema =
          Schema
          { _schemaType = st
          , _schemaEnum = Just $ AnyKeywordEnum (Aeson.String <$> values)
          , _schemaConst = Nothing
          , _schemaRequired = Nothing
          , _schemaProperties = Nothing
          }
    v <- forAll $ genConstrainedJSON ranges schema
    assert $ isJust $ find (== Aeson.decodeStrict v) (Just <$> values)

prop_constrainedValueFromConst :: Property
prop_constrainedValueFromConst =
  property $ do
    st <- forAll genTypes
    c <- forAll $ genJSONValue ranges
    let schema =
          Schema
          { _schemaType = st
          , _schemaEnum = Nothing
          , _schemaConst = Just $ AnyKeywordConst c
          , _schemaRequired = Nothing
          , _schemaProperties = Nothing
          }
    v <- forAll $ genConstrainedJSON ranges schema
    Aeson.decodeStrict v === Just c

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
      , _schemaRequired = Just (ObjectKeywordRequired $ S.fromList ["user_id"])
      , _schemaProperties =
          (Just . ObjectKeywordProperties)
            (H.fromList
               [ ( "user_id"
                 , Schema
                   { _schemaType = SingleType IntegerType
                   , _schemaEnum = Nothing
                   , _schemaConst = Nothing
                   , _schemaProperties = Nothing
                   , _schemaRequired = Nothing
                   })
               , ( "user_domain"
                 , Schema
                   { _schemaType = SingleType StringType
                   , _schemaEnum = Nothing
                   , _schemaConst = Nothing
                   , _schemaProperties = Nothing
                   , _schemaRequired = Nothing
                   })
               ])
      }

tests :: TestTree
tests =
  testGroup
    "Hedgehog.Gen.JSON tests"
    [ testProperty "Generated unconstrained values are valid JSON" prop_generatedUnconstrainedJSON
    , testProperty "Decodes JSON Schema" prop_decodesSchema
    , testProperty "Generates constrained values from JSON Schema enum when present" prop_constrainedValueFromEnum
    , testProperty "Generates constrained values from JSON Schema const when present" prop_constrainedValueFromConst
    ]

main :: IO ()
main = defaultMain tests
