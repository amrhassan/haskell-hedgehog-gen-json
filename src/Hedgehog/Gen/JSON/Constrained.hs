{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Gen.JSON.Constrained
  ( genValue
  , Schema
  ) where

import           Control.Lens
import qualified Data.Aeson                   as Aeson
import           Data.Fixed                   (mod')
import qualified Data.Scientific              as Scientific
import qualified Data.Text                    as Text
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import           Hedgehog.Gen.JSON.JSONSchema
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Range               as Range
import           Protolude
import qualified Regex.Genex                  as Genex

genValue :: Ranges -> Schema -> Gen Aeson.Value
genValue ranges schema
  | isJust (schema ^. schemaEnum) =
    case schema ^. schemaEnum of
      Just (AnyConstraintEnum vs) -> (Gen.element . toList) vs
      Nothing                     -> empty
  | isJust (schema ^. schemaConst) =
    case schema ^. schemaConst of
      Just (AnyConstraintConst c) -> pure c
      Nothing                     -> empty
  | otherwise =
    case schema ^. schemaType of
      MultipleTypes (t :| []) -> genValue ranges (over schemaType (const $ SingleType t) schema)
      MultipleTypes (t :| [t']) ->
        Gen.choice
          [ genValue ranges (over schemaType (const $ SingleType t) schema)
          , genValue ranges (over schemaType (const $ SingleType t') schema)
          ]
      MultipleTypes (t :| (t':ts)) ->
        Gen.choice
          [ genValue ranges (over schemaType (const $ SingleType t) schema)
          , genValue ranges (over schemaType (const $ MultipleTypes (t' :| ts)) schema)
          ]
      SingleType NullType -> genNullValue
      SingleType BooleanType -> genBooleanValue
      SingleType NumberType -> genNumberValue (ranges ^. numberRange) schema
      SingleType IntegerType -> genIntegerValue schema
      SingleType StringType -> genString schema

genNullValue :: Gen Aeson.Value
genNullValue = Gen.constant Aeson.Null

genBooleanValue :: Gen Aeson.Value
genBooleanValue = Aeson.Bool <$> Gen.bool

genNumberValue :: NumberRange -> Schema -> Gen Aeson.Value
genNumberValue (NumberRange r) schema =
  (Aeson.Number . Scientific.fromFloatDigits) <$>
  Gen.double (Range.linearFrac (Scientific.toRealFloat vmin) (Scientific.toRealFloat vmax))
  where
    defaultMin = Scientific.fromFloatDigits $ Range.lowerBound (-5000) r
    defaultMax = Scientific.fromFloatDigits $ Range.upperBound 5000 r
    vmin =
      case (schema ^. schemaMinimum, schema ^. schemaExclusiveMinimum) of
        (Just (NumberConstraintMinimum x), Just (NumberConstraintExclusiveMinimum y)) -> max x (y + 0.1)
        (Just (NumberConstraintMinimum x), Nothing) -> x
        (Nothing, Just (NumberConstraintExclusiveMinimum y)) -> (y + 0.1)
        (Nothing, Nothing) -> defaultMin
    vmax =
      case (schema ^. schemaMaximum, schema ^. schemaExclusiveMaximum) of
        (Just (NumberConstraintMaximum x), Just (NumberConstraintExclusiveMaximum y)) -> min x (y - 0.1)
        (Just (NumberConstraintMaximum x), Nothing) -> x
        (Nothing, Just (NumberConstraintExclusiveMaximum y)) -> (y - 0.1)
        (Nothing, Nothing) -> defaultMax

genIntegerValue :: Schema -> Gen Aeson.Value
genIntegerValue schema =
  Aeson.Number <$>
  (Gen.filter multipleOfPredicate $
   (fromInteger . round) <$> Gen.double (Range.linearFrac (Scientific.toRealFloat vmin) (Scientific.toRealFloat vmax)))
  where
    defaultMin = -5000
    defaultMax = 5000
    vmin =
      case (schema ^. schemaMinimum, schema ^. schemaExclusiveMinimum) of
        (Just (NumberConstraintMinimum x), Just (NumberConstraintExclusiveMinimum y)) -> max x (y + 1)
        (Just (NumberConstraintMinimum x), Nothing) -> x
        (Nothing, Just (NumberConstraintExclusiveMinimum y)) -> (y + 1)
        (Nothing, Nothing) -> defaultMin
    vmax =
      case (schema ^. schemaMaximum, schema ^. schemaExclusiveMaximum) of
        (Just (NumberConstraintMaximum x), Just (NumberConstraintExclusiveMaximum y)) -> min x (y - 1)
        (Just (NumberConstraintMaximum x), Nothing) -> x
        (Nothing, Just (NumberConstraintExclusiveMaximum y)) -> (y - 1)
        (Nothing, Nothing) -> defaultMax
    multipleOfPredicate =
      case schema ^. schemaMultipleOf of
        Just (NumberConstraintMultipleOf x) -> \m -> m `mod'` x == 0
        Nothing                             -> const True

genString :: Schema -> Gen Aeson.Value
genString schema =
  case schema ^. schemaPattern of
    Just (StringConstraintPattern x) ->
      Gen.element $ (Aeson.String . Text.pack) <$> take 10 (Genex.genexPure [Text.unpack x])
    Nothing -> Aeson.String <$> Gen.text (Range.linear minLength maxLength) Gen.unicode
  where
    minLength =
      case schema ^. schemaMinLength of
        Just (StringConstraintMinLength x) -> x
        Nothing                            -> 0
    maxLength =
      case schema ^. schemaMaxLength of
        Just (StringConstraintMaxLength x) -> x
        Nothing                            -> 1000
