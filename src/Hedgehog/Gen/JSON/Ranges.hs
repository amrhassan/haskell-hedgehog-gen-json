{-# LANGUAGE TemplateHaskell #-}

module Hedgehog.Gen.JSON.Ranges where

import           Control.Lens
import           Hedgehog

newtype NumberRange = NumberRange (Range Double)

newtype StringRange = StringRange (Range Int)

newtype ArrayRange = ArrayRange (Range Int)

newtype ObjectRange = ObjectRange (Range Int)

data Ranges = Ranges
  { _numberRange :: NumberRange
  , _stringRange :: StringRange
  , _arrayRange  :: ArrayRange
  , _objectRange :: ObjectRange
  }

makeLenses ''Ranges
