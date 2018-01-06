{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Gen.JSON.Constrained.Internal
  ( genBoundedInteger
  , genBoundedReal
  , genBoundedString
  , genStringFromRegexp
  , genUniqueItems
  , filterAll
  , filterAllMaybe
  ) where

import qualified Data.HashSet                 as HashSet
import           Data.Scientific              (Scientific)
import qualified Data.Scientific              as Scientific
import qualified Data.Text                    as Text
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import           Hedgehog.Gen.JSON.JSONSchema
import qualified Hedgehog.Range               as Range
import           Protolude
import qualified Regex.Genex                  as Genex

-- | Generates an Integer bounded by the given constraints or by the given range, as well as a multiple-of constraint.
genBoundedInteger ::
     Maybe NumberConstraintExclusiveMinimum
  -> Maybe NumberConstraintMinimum
  -> Maybe NumberConstraintExclusiveMaximum
  -> Maybe NumberConstraintMaximum
  -> Maybe NumberConstraintMultipleOf
  -> Range Integer
  -> Gen Integer
genBoundedInteger cminEx cmin cmaxEx cmax cmult range =
  Gen.sized $ \sz -> filterAllMaybe filters $ Gen.integral (finalRange sz)
  where
    filters = [minExFilter, maxExFilter, minFilter, maxFilter, multipleOfFilter]
    minExFilter = ((\b -> (> b)) . truncateScientific . unNumberConstraintExclusiveMinimum) <$> cminEx
    minFilter = ((\b -> (>= b)) . truncateScientific . unNumberConstraintMinimum) <$> cmin
    maxExFilter = ((\b -> (< b)) . truncateScientific . unNumberConstraintExclusiveMaximum) <$> cmaxEx
    maxFilter = ((\b -> (<= b)) . truncateScientific . unNumberConstraintMaximum) <$> cmax
    multipleOfFilter = ((\m x -> (x `rem` m == 0)) . truncateScientific . unNumberConstraintMultipleOf) <$> cmult
    finalRange sz = Range.linear (minB sz) (maxB sz)
    minB sz =
      fromMaybe
        (Range.lowerBound sz range)
        ((maximumMay . catMaybes)
           [ (truncateScientific . unNumberConstraintMinimum) <$> cmin
           , (truncateScientific . unNumberConstraintExclusiveMinimum) <$> cminEx
           ])
    maxB sz =
      fromMaybe
        (Range.lowerBound sz range)
        ((minimumMay . catMaybes)
           [ (truncateScientific . unNumberConstraintMaximum) <$> cmax
           , (truncateScientific . unNumberConstraintExclusiveMaximum) <$> cmaxEx
           ])

-- | Generates a Double bounded by the given constraints or by the given range.
genBoundedReal ::
     Maybe NumberConstraintExclusiveMinimum
  -> Maybe NumberConstraintMinimum
  -> Maybe NumberConstraintExclusiveMaximum
  -> Maybe NumberConstraintMaximum
  -> Range Double
  -> Gen Double
genBoundedReal cminEx cmin cmaxEx cmax range = Gen.sized $ \sz -> filterAllMaybe filters $ Gen.realFloat (finalRange sz)
  where
    filters = [minExFilter, maxExFilter, minFilter, maxFilter]
    minExFilter = ((\b -> (> b)) . Scientific.toRealFloat . unNumberConstraintExclusiveMinimum) <$> cminEx
    minFilter = ((\b -> (>= b)) . Scientific.toRealFloat . unNumberConstraintMinimum) <$> cmin
    maxExFilter = ((\b -> (< b)) . Scientific.toRealFloat . unNumberConstraintExclusiveMaximum) <$> cmaxEx
    maxFilter = ((\b -> (<= b)) . Scientific.toRealFloat . unNumberConstraintMaximum) <$> cmax
    finalRange sz = Range.linearFrac (minB sz) (maxB sz)
    minB sz =
      fromMaybe
        (Range.lowerBound sz range)
        ((maximumMay . catMaybes)
           [ (Scientific.toRealFloat . unNumberConstraintMinimum) <$> cmin
           , (Scientific.toRealFloat . unNumberConstraintExclusiveMinimum) <$> cminEx
           ])
    maxB sz =
      fromMaybe
        (Range.lowerBound sz range)
        ((minimumMay . catMaybes)
           [ (Scientific.toRealFloat . unNumberConstraintMaximum) <$> cmax
           , (Scientific.toRealFloat . unNumberConstraintExclusiveMaximum) <$> cmaxEx
           ])

-- | Generates a Text bounded in size by the given constraints or by the given range.
genBoundedString :: Maybe StringConstraintMinLength -> Maybe StringConstraintMaxLength -> Range Int -> Gen Text
genBoundedString minLengthC maxLengthC range =
  Gen.sized $ \size -> filterAllMaybe filters $ Gen.text (Range.linear (minB size) (maxB size)) Gen.unicode
  where
    minB sz = maybe (Range.lowerBound sz range) unStringConstraintMinLength minLengthC
    maxB sz = maybe (Range.upperBound sz range) unStringConstraintMaxLength maxLengthC
    filters =
      [ ((\b t -> Text.length t >= b) . unStringConstraintMinLength) <$> minLengthC
      , ((\b t -> Text.length t <= b) . unStringConstraintMaxLength) <$> maxLengthC
      ]

-- | Generates a Text from a given Regular Expression
genStringFromRegexp :: Text -> Gen Text
genStringFromRegexp regexp = Gen.element $ Text.pack <$> take 10 (Genex.genexPure [Text.unpack regexp])

-- | Generates unique lists of the given generator and a size range
genUniqueItems :: (Hashable a, Eq a) => Range Int -> Gen a -> Gen [a]
genUniqueItems = genUniqueItems' 100 []

-- Gives up after 100 trials
genUniqueItems' :: (Hashable a, Eq a) => Int -> [a] -> Range Int -> Gen a -> Gen [a]
genUniqueItems' s acc range gen
  | s == 0 = Gen.discard
  | otherwise =
    Gen.sized $ \size -> do
      initialList <- (makeListUnique . (++ acc)) <$> Gen.list range gen
      let l = length initialList
      if Range.lowerBound size range <= l
        then pure initialList
        else take (Range.upperBound size range) <$> genUniqueItems' (s - 1) initialList range gen

-- | Filters out values that do not satisfy all the given predicates each
filterAll :: [a -> Bool] -> Gen a -> Gen a
filterAll filters = Gen.filter (\e -> and (($ e) <$> filters))

-- | Filters out values that do not satisfy all the given not-Nothing predicates
filterAllMaybe :: [Maybe (a -> Bool)] -> Gen a -> Gen a
filterAllMaybe filters = filterAll (catMaybes filters)

--- Helpers ----
truncateScientific :: Integral a => Scientific -> a
truncateScientific x = truncate real
  where
    real :: Double = Scientific.toRealFloat x

makeListUnique :: (Eq a, Hashable a) => [a] -> [a]
makeListUnique = toList . HashSet.fromList
