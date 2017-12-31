{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Gen.JSON.Constrained
  ( genValue
  , Schema
  ) where

import           Control.Lens
import qualified Data.Aeson                      as Aeson
import           Data.Fixed                      (mod')
import qualified Data.HashMap.Strict             as HM
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet                    as HashSet
import qualified Data.Scientific                 as Scientific
import qualified Data.Text                       as Text
import qualified Data.Vector                     as Vector
import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import           Hedgehog.Gen.JSON.JSONSchema
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import qualified Hedgehog.Range                  as Range
import           Protolude
import qualified Regex.Genex                     as Genex

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
      SingleType IntegerType -> genIntegerValue (ranges ^. numberRange) schema
      SingleType StringType -> genString (ranges ^. stringRange) schema
      SingleType ObjectType -> genObject ranges schema
      SingleType ArrayType -> genArray ranges schema

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

genIntegerValue :: NumberRange -> Schema -> Gen Aeson.Value
genIntegerValue (NumberRange nr) schema =
  Aeson.Number <$>
  (Gen.filter multipleOfPredicate $
   (fromInteger . round) <$> Gen.double (Range.linearFrac (Scientific.toRealFloat vmin) (Scientific.toRealFloat vmax)))
  where
    defaultMin = Scientific.fromFloatDigits $ Range.lowerBound (-5000) nr
    defaultMax = Scientific.fromFloatDigits $ Range.upperBound 5000 nr
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

genString :: StringRange -> Schema -> Gen Aeson.Value
genString (StringRange sr) schema =
  case schema ^. schemaPattern of
    Just (StringConstraintPattern x) ->
      Gen.element $ (Aeson.String . Text.pack) <$> take 10 (Genex.genexPure [Text.unpack x])
    Nothing -> Aeson.String <$> Gen.text (Range.linear minLength maxLength) Gen.unicode
  where
    minLength =
      case schema ^. schemaMinLength of
        Just (StringConstraintMinLength x) -> x
        Nothing                            -> Range.lowerBound 0 sr
    maxLength =
      case schema ^. schemaMaxLength of
        Just (StringConstraintMaxLength x) -> x
        Nothing                            -> Range.upperBound 1000 sr

genObject :: Ranges -> Schema -> Gen Aeson.Value
genObject ranges schema = (Aeson.Object . HM.fromList . join) <$> generatedFields
  where
    generatedFields = traverse (\(n, gen) -> (\m -> (\v -> (n, v)) <$> (maybeToList m)) <$> gen) generatedFieldsMaybes
    generatedFieldsMaybes =
      (\(n, s) ->
         ( n
         , (if n `elem` required
              then fmap Just
              else Gen.maybe)
             (Gen.small $ genValue ranges s))) <$>
      HM.toList properties
    ObjectConstraintRequired required = fromMaybe (ObjectConstraintRequired []) $ schema ^. schemaRequired
    ObjectConstraintProperties properties = fromMaybe (ObjectConstraintProperties HM.empty) $ schema ^. schemaProperties

genArray :: Ranges -> Schema -> Gen Aeson.Value
genArray ranges schema =
  case itemSchemaMaybe of
    Just itemSchema ->
      (Aeson.Array . Vector.fromList) <$>
      (if uniqueItems
         then (toList <$> genUniqueList finalRange (Gen.small $ genValue ranges itemSchema))
         else (Gen.list finalRange (Gen.small $ genValue ranges itemSchema)))
    Nothing -> Unconstrained.genArray ranges
  where
    finalRange = Range.linear (fromMaybe 0 minItems) (fromMaybe 10 maxItems)
    itemSchemaMaybe = (\(ArrayConstraintItems items) -> items) <$> (schema ^. schemaItems)
    ArrayConstraintUniqueItems uniqueItems = fromMaybe (ArrayConstraintUniqueItems False) (schema ^. schemaUniqueItems)
    maxItems = (\(ArrayConstraintMaxItems n) -> n) <$> (schema ^. schemaMaxItems)
    minItems = (\(ArrayConstraintMinItems n) -> n) <$> (schema ^. schemaMinItems)

uniqueList :: (Eq a, Hashable a) => [a] -> [a]
uniqueList = toList . HashSet.fromList

genUniqueList :: (MonadGen m, Hashable a, Eq a) => Range Int -> m a -> m [a]
genUniqueList range gen =
  Gen.sized $ \size -> do
    initialList <- initialListGen
    if length initialList < Range.lowerBound size range
      then (\moreList -> take (Range.upperBound size range) (initialList ++ moreList)) <$> moreListGen
      else pure initialList
  where
    initialListGen = uniqueList <$> Gen.list range gen
    moreListGen = Gen.small $ genUniqueList range gen
