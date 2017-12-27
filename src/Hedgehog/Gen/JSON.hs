{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hedgehog.Gen.JSON
  ( genValue
  , Ranges(..)
  , NumberRange(..)
  , StringRange(..)
  , ArrayRange(..)
  , ObjectRange(..)
  ) where

import           Control.Lens
import           Data.Aeson        (Value, decodeStrict)
import qualified Data.Aeson        as A
import qualified Data.ByteString   as BS
import qualified Data.Scientific   as Scientific
import qualified Data.Vector       as Vector
import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified JSONSchema.Draft4 as D4
import           Protolude

newtype NumberRange = NumberRange
  { unNumberRange :: Range Double
  }

newtype StringRange = StringRange
  { unStringRange :: Range Int
  }

newtype ArrayRange = ArrayRange
  { unArrayRange :: Range Int
  }

newtype ObjectRange = ObjectRange
  { unObjectRange :: Range Int
  }

data Ranges = Ranges
  { _numberRange :: NumberRange
  , _stringRange :: StringRange
  , _arrayRange  :: ArrayRange
  , _objectRange :: ObjectRange
  }

makeLenses ''Ranges

readSchema :: FilePath -> IO (Either Text D4.Schema)
readSchema fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (decodeStrict bytes)

genNull :: Gen Value
genNull = pure A.Null

genString :: StringRange -> Gen Value
genString sr = A.String <$> Gen.text (unStringRange sr) Gen.unicode

genBool :: Gen Value
genBool = A.Bool <$> Gen.bool

genNumber :: NumberRange -> Gen Value
genNumber nr = (A.Number . Scientific.fromFloatDigits) <$> Gen.double (unNumberRange nr)

genArray :: Ranges -> Gen Value
genArray ranges = do
  let gen =
        Gen.recursive
          Gen.choice
          [genBool, genNumber (ranges ^. numberRange), genString (ranges ^. stringRange)]
          [genArray ranges, genObj ranges]
  (A.Array . Vector.fromList) <$> Gen.list (unArrayRange (ranges ^. arrayRange)) gen

genObj :: Ranges -> Gen Value
genObj ranges =
  A.object <$>
  Gen.list
    (unArrayRange (ranges ^. arrayRange))
    ((,) <$> Gen.text (unStringRange (ranges ^. stringRange)) Gen.unicode <*> genValue ranges)

genValue :: Ranges -> Gen Value
genValue ranges =
  Gen.choice
    [ genNull
    , genString (ranges ^. stringRange)
    , genBool
    , genNumber (ranges ^. numberRange)
    , genArray ranges
    , genObj ranges
    ]

--genFakeJson :: D4.Schema -> Gen Value
--genFakeJson = undefined
