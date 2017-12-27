{-# LANGUAGE TemplateHaskell #-}

module Hedgehog.Gen.JSON.Ranges where

import           Control.Lens
import           Hedgehog

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
