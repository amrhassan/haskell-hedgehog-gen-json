module Hedgehog.Gen.JSON.Unconstrained where

import           Control.Lens
import qualified Data.Aeson               as A
import qualified Data.Scientific          as Scientific
import qualified Data.Vector              as Vector
import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import           Hedgehog.Gen.JSON.Ranges

genNull :: Gen A.Value
genNull = pure A.Null

genString :: StringRange -> Gen A.Value
genString sr = A.String <$> Gen.text (unStringRange sr) Gen.unicode

genBool :: Gen A.Value
genBool = A.Bool <$> Gen.bool

genNumber :: NumberRange -> Gen A.Value
genNumber nr = (A.Number . Scientific.fromFloatDigits) <$> Gen.double (unNumberRange nr)

genArray :: Ranges -> Gen A.Value
genArray ranges = do
  let gen = Gen.recursive Gen.choice [genBool, genNumber nr, genString sr] [genArray ranges, genObj ranges]
  (A.Array . Vector.fromList) <$> Gen.list (unArrayRange ar) gen
  where
    nr = ranges ^. numberRange
    sr = ranges ^. stringRange
    ar = ranges ^. arrayRange

genObj :: Ranges -> Gen A.Value
genObj ranges = A.object <$> Gen.list ar ((,) <$> Gen.text sr Gen.unicode <*> genValue ranges)
  where
    sr = unStringRange (ranges ^. stringRange)
    ar = unArrayRange (ranges ^. arrayRange)

genValue :: Ranges -> Gen A.Value
genValue ranges =
  Gen.choice
    [ genNull
    , genString (ranges ^. stringRange)
    , genBool
    , genNumber (ranges ^. numberRange)
    , genArray ranges
    , genObj ranges
    ]
