{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Gen.JSON.Constrained
  ( genValue
  , Schema
  ) where

import           Control.Lens
import qualified Data.Aeson                      as Aeson
import           Data.Fixed                      (mod')
import qualified Data.HashMap.Strict             as HM
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
      Nothing -> Unconstrained.genValue ranges
      Just (MultipleTypes (t :| [])) -> genValue ranges (set schemaType (Just $ SingleType t) schema)
      Just (MultipleTypes (t :| [t'])) ->
        Gen.choice
          [ genValue ranges (set schemaType (Just $ SingleType t) schema)
          , genValue ranges (set schemaType (Just $ SingleType t') schema)
          ]
      Just (MultipleTypes (t :| (t':ts))) ->
        Gen.choice
          [ genValue ranges (set schemaType (Just $ SingleType t) schema)
          , genValue ranges (set schemaType (Just $ MultipleTypes (t' :| ts)) schema)
          ]
      Just (SingleType NullType) -> genNullValue
      Just (SingleType BooleanType) -> genBooleanValue
      Just (SingleType NumberType) -> genNumberValue (ranges ^. numberRange) schema
      Just (SingleType IntegerType) -> genIntegerValue (ranges ^. numberRange) schema
      Just (SingleType StringType) -> genString (ranges ^. stringRange) schema
      Just (SingleType ObjectType) -> genObject ranges schema
      Just (SingleType ArrayType) -> genArray ranges schema

genNullValue :: Gen Aeson.Value
genNullValue = Gen.constant Aeson.Null

genBooleanValue :: Gen Aeson.Value
genBooleanValue = Aeson.Bool <$> Gen.bool

genNumberValue :: NumberRange -> Schema -> Gen Aeson.Value
genNumberValue nr schema = (Aeson.Number . Scientific.fromFloatDigits) <$> genBoundedNumber nr schema

genIntegerValue :: NumberRange -> Schema -> Gen Aeson.Value
genIntegerValue nr schema = do
  let boundedNumber = (fromInteger . round) <$> genBoundedNumber nr schema
  Aeson.Number <$> Gen.filter multipleOfPredicate boundedNumber
  where
    multipleOfPredicate =
      case schema ^. schemaMultipleOf of
        Just (NumberConstraintMultipleOf x) -> \m -> m `mod'` x == 0
        Nothing                             -> const True

-- Based on min/max/minex/maxex bounds from the Schema as well as the passed range
genBoundedNumber :: NumberRange -> Schema -> Gen Double
genBoundedNumber (NumberRange nr) schema =
  Gen.sized $ \size ->
    let range = Range.linearFrac (Scientific.toRealFloat $ vmin size) (Scientific.toRealFloat $ vmax size)
    in Gen.double range
  where
    vmin sz =
      case (schema ^. schemaMinimum, schema ^. schemaExclusiveMinimum) of
        (Just (NumberConstraintMinimum x), Just (NumberConstraintExclusiveMinimum y)) -> max x (y + 1)
        (Just (NumberConstraintMinimum x), Nothing) -> x
        (Nothing, Just (NumberConstraintExclusiveMinimum y)) -> y + 2
        (Nothing, Nothing) -> Scientific.fromFloatDigits $ Range.lowerBound sz nr
    vmax sz =
      case (schema ^. schemaMaximum, schema ^. schemaExclusiveMaximum) of
        (Just (NumberConstraintMaximum x), Just (NumberConstraintExclusiveMaximum y)) -> min x (y - 1)
        (Just (NumberConstraintMaximum x), Nothing) -> x
        (Nothing, Just (NumberConstraintExclusiveMaximum y)) -> y - 1
        (Nothing, Nothing) -> Scientific.fromFloatDigits $ Range.upperBound sz nr

genString :: StringRange -> Schema -> Gen Aeson.Value
genString (StringRange sr) schema =
  Gen.sized $ \size ->
    case schema ^. schemaPattern of
      Just (StringConstraintPattern x) ->
        Gen.element $ (Aeson.String . Text.pack) <$> take 10 (Genex.genexPure [Text.unpack x])
      Nothing -> Aeson.String <$> Gen.text (Range.linear (minLength size) (maxLength size)) Gen.unicode
  where
    minLength sz =
      case schema ^. schemaMinLength of
        Just (StringConstraintMinLength x) -> x
        Nothing                            -> Range.lowerBound sz sr
    maxLength sz =
      case schema ^. schemaMaxLength of
        Just (StringConstraintMaxLength x) -> x
        Nothing                            -> Range.upperBound sz sr

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
