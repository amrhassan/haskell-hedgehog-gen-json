{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens                 ((^.))
import qualified Data.Aeson                   as Aeson
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as H
import qualified Data.Set                     as S
import           Hedgehog
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

prop_generatedUnconstrainedJSON :: Property
prop_generatedUnconstrainedJSON =
  property $ do
    v <- forAll $ genJSON ranges
    assert $ isJust (Aeson.decodeStrict v :: Maybe Aeson.Value)

prop_decodesSchema :: Property
prop_decodesSchema = property $ decoded === Right expected
  where
    schemaJson =
      "{\"type\":\"object\",\"properties\":{\"user_id\":{\"type\":\"integer\"},\"user_domain\":{\"type\":\"string\"}},\"required\":[\"user_id\"]}"
    decoded :: Either P.String Schema = Aeson.eitherDecode schemaJson
    expected =
        Schema
        { _schemaType = SingleType ObjectType
        , _enum = Nothing
        , _const = Nothing
        , _required = Just (ObjectKeywordRequired $ S.fromList ["user_id"])
        , _properties =
            (Just . ObjectKeywordProperties)
              (H.fromList
                 [ ( "user_id"
                   , Schema
                     { _schemaType = SingleType IntegerType
                     , _enum = Nothing
                     , _const = Nothing
                     , _properties = Nothing
                     , _required = Nothing
                     })
                 , ( "user_domain"
                   , Schema
                     { _schemaType = SingleType StringType
                     , _enum = Nothing
                     , _const = Nothing
                     , _properties = Nothing
                     , _required = Nothing
                     })
                 ])
        }

tests :: TestTree
tests =
  testGroup
    "Hedgehog.Gen.JSON tests"
    [ testProperty "Generated unconstrained values are valid JSON" prop_generatedUnconstrainedJSON
    , testProperty "Decodes JSON Schema" prop_decodesSchema
    ]

main :: IO ()
main = defaultMain tests
