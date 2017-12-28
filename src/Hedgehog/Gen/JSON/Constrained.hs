{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hedgehog.Gen.JSON.Constrained where

import           Control.Lens
import qualified Data.Aeson                      as A
import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import           Hedgehog.Gen.JSON.JSONSchema    (Schema(..))
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import           Protolude

genValue :: Ranges -> Schema -> Gen A.Value
genValue = undefined

genAnyFromEnum :: NonEmpty A.Value -> Gen A.Value
genAnyFromEnum = Gen.element . toList
