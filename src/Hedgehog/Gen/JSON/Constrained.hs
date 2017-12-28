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
      Just (AnyKeywordEnum vs) -> (Gen.element . toList) vs
      Nothing                  -> empty
  | isJust (schema ^. schemaConst) =
    case schema ^. schemaConst of
      Just (AnyKeywordConst c) -> pure c
      Nothing                  -> empty
  | otherwise = undefined -- TODO
