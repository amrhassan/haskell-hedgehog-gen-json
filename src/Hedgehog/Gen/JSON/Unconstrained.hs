module Hedgehog.Gen.JSON.Unconstrained where

import           Control.Lens
import qualified Data.Aeson               as A
import qualified Data.Scientific          as Scientific
import qualified Data.Vector              as Vector
import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import           Hedgehog.Gen.JSON.Ranges
import           Protolude

genNull :: Gen A.Value
genNull = pure A.Null

genStringValue :: StringRange -> Gen A.Value
genStringValue (StringRange sr) = A.String <$> Gen.text sr Gen.unicode

genBool :: Gen A.Value
genBool = A.Bool <$> Gen.bool

genNumber :: NumberRange -> Gen A.Value
genNumber (NumberRange nr) = (A.Number . Scientific.fromFloatDigits) <$> Gen.double nr

genArray :: Ranges -> Gen A.Value
genArray ranges = do
  let gen = Gen.recursive Gen.choice [genBool, genNumber nr, genStringValue sr] [genArray ranges, genObj ranges]
  (A.Array . Vector.fromList) <$> Gen.list ar gen
  where
    nr = ranges ^. numberRange
    sr = ranges ^. stringRange
    ArrayRange ar = ranges ^. arrayRange

genObj :: Ranges -> Gen A.Value
genObj ranges = A.object <$> Gen.list ar ((,) <$> Gen.text sr Gen.unicode <*> genValue ranges)
  where
    (StringRange sr) = ranges ^. stringRange
    (ArrayRange ar) = ranges ^. arrayRange

genValue :: Ranges -> Gen A.Value
genValue ranges = Gen.choice [genNull, genStringValue (ranges ^. stringRange), genBool, genNumber (ranges ^. numberRange), genArray ranges, genObj ranges]
