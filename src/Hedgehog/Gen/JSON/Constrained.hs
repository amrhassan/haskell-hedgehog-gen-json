{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hedgehog.Gen.JSON.Constrained where

import           Control.Lens
import qualified Data.Aeson                      as A
import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import qualified JSONSchema.Draft4               as D4
import qualified JSONSchema.Validator.Draft4     as D4V
import           Protolude

genValue :: Ranges -> D4.Schema -> Gen A.Value
genValue = undefined

genAnyFromEnum :: NonEmpty A.Value -> Gen A.Value
genAnyFromEnum = Gen.element . toList
