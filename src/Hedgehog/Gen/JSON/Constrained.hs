{-# LANGUAGE NoImplicitPrelude #-}

module Hedgehog.Gen.JSON.Constrained
  ( genValue
  , Schema
  ) where

import           Control.Lens
import qualified Data.Aeson                      as Aeson
import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import           Hedgehog.Gen.JSON.JSONSchema
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import           Protolude

genValue :: Ranges -> Schema -> Gen Aeson.Value
genValue ranges schema
  | isJust (schema ^. schemaEnum) =
    case schema ^. schemaEnum of
      Just (AnyKeywordEnum vs) -> genValueFromEnum vs
      Nothing                  -> Gen.element []
  | otherwise = undefined -- TODO

genValueFromEnum :: NonEmpty Aeson.Value -> Gen Aeson.Value
genValueFromEnum = Gen.element . toList
