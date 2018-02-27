{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Gen.JSON.Constrained
  ( genValue
  , Schema
  ) where

import           Control.Lens
import qualified Data.Aeson                             as Aeson
import qualified Data.HashMap.Strict                    as HashMap
import qualified Data.Scientific                        as Scientific
import           Data.Time.RFC3339
import qualified Data.Vector                            as Vector
import           Hedgehog
import qualified Hedgehog.Gen                           as Gen
import           Hedgehog.Gen.JSON.Constrained.Internal
import           Hedgehog.Gen.JSON.JSONSchema
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained        as Unconstrained
import qualified Hedgehog.Range                         as Range
import           Protolude

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
      Nothing -> Unconstrained.genValue ranges
      Just (MultipleTypes (t :| [])) -> genValue ranges (set schemaType (Just $ SingleType t) schema)
      Just (MultipleTypes (t :| [t'])) ->
        Gen.choice [genValue ranges (set schemaType (Just $ SingleType t) schema), genValue ranges (set schemaType (Just $ SingleType t') schema)]
      Just (MultipleTypes (t :| (t':ts))) ->
        Gen.choice [genValue ranges (set schemaType (Just $ SingleType t) schema), genValue ranges (set schemaType (Just $ MultipleTypes (t' :| ts)) schema)]
      Just (SingleType NullType) -> genNullValue
      Just (SingleType BooleanType) -> genBooleanValue
      Just (SingleType NumberType) -> genNumberValue (ranges ^. numberRange) schema
      Just (SingleType IntegerType) -> genIntegerValue (ranges ^. integerRange) schema
      Just (SingleType StringType) -> genStringValue (ranges ^. stringRange) schema
      Just (SingleType ObjectType) -> genObjectValue ranges schema
      Just (SingleType ArrayType) -> genArrayValue ranges schema

genNullValue :: Gen Aeson.Value
genNullValue = Gen.constant Aeson.Null

genBooleanValue :: Gen Aeson.Value
genBooleanValue = Aeson.Bool <$> Gen.bool

genNumberValue :: NumberRange -> Schema -> Gen Aeson.Value
genNumberValue (NumberRange nr) schema =
  (Aeson.Number . Scientific.fromFloatDigits) <$>
  genBoundedReal (schema ^. schemaExclusiveMinimum) (schema ^. schemaMinimum) (schema ^. schemaExclusiveMaximum) (schema ^. schemaMaximum) nr

genIntegerValue :: IntegerRange -> Schema -> Gen Aeson.Value
genIntegerValue (IntegerRange nr) schema =
  (Aeson.Number . fromInteger) <$>
  genBoundedInteger
    (schema ^. schemaExclusiveMinimum)
    (schema ^. schemaMinimum)
    (schema ^. schemaExclusiveMaximum)
    (schema ^. schemaMaximum)
    (schema ^. schemaMultipleOf)
    nr

genStringValue :: StringRange -> Schema -> Gen Aeson.Value
genStringValue (StringRange sr) schema =
  case schema ^. schemaPattern of
    Just (StringConstraintPattern regexp) -> Aeson.String <$> genStringFromRegexp regexp
    Nothing ->
      case schema ^. schemaFormat of
        Just (StringConstraintFormat f) -> genWithFormat f
        Nothing                         -> genUnformatted
  where
    genWithFormat "uuid" = Aeson.String <$> genStringFromRegexp "[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}"
    genWithFormat "date-time" = (Aeson.String . formatTimeRFC3339) <$> genZonedTime (Range.linearFrac 0 999999999999999999)
    genWithFormat "RFC 3339 date-time" = genWithFormat "date-time"
    genWithFormat _ = genUnformatted
    genUnformatted = Aeson.String <$> genBoundedString (schema ^. schemaMinLength) (schema ^. schemaMaxLength) sr

genObjectValue :: Ranges -> Schema -> Gen Aeson.Value
genObjectValue ranges schema = (Aeson.Object . HashMap.fromList . join) <$> generatedFields
  where
    generatedFields = traverse (\(n, gen) -> (\m -> (\v -> (n, v)) <$> (maybeToList m)) <$> gen) generatedFieldsMaybes
    generatedFieldsMaybes =
      (\(n, s) ->
         ( n
         , (if n `elem` required
              then fmap Just
              else Gen.maybe)
             (Gen.small $ genValue ranges s))) <$>
      HashMap.toList properties
    required = maybe [] unObjectConstraintRequired (schema ^. schemaRequired)
    properties = maybe HashMap.empty unObjectConstraintProperties (schema ^. schemaProperties)

genArrayValue :: Ranges -> Schema -> Gen Aeson.Value
genArrayValue ranges schema =
  case unArrayConstraintItems <$> (schema ^. schemaItems) of
    Just itemSchema ->
      Gen.sized $ \sz ->
        (Aeson.Array . Vector.fromList) <$>
        let listMaker =
              if uniqueItems
                then genUniqueItems
                else Gen.list
         in listMaker (finalRange sz) (Gen.small $ genValue ranges itemSchema)
    Nothing -> Unconstrained.genArray ranges
  where
    ar = unArrayRange (ranges ^. arrayRange)
    finalRange sz = Range.linear (fromMaybe (Range.lowerBound sz ar) minItems) (fromMaybe (Range.upperBound sz ar) maxItems)
    uniqueItems = maybe False unArrayConstraintUniqueItems (schema ^. schemaUniqueItems)
    maxItems = unArrayConstraintMaxItems <$> (schema ^. schemaMaxItems)
    minItems = unArrayConstraintMinItems <$> (schema ^. schemaMinItems)
