{-# LANGUAGE NoImplicitPrelude #-}

module Hedgehog.Gen.JSON.Constrained
  ( genValue
  , Schema
  ) where

import           Control.Lens
import qualified Data.Aeson                   as Aeson
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import           Hedgehog.Gen.JSON.JSONSchema
import           Hedgehog.Gen.JSON.Ranges
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
  | otherwise =
    case schema ^. schemaType of
      MultipleTypes (t :| []) -> genValue ranges (over schemaType (const $ SingleType t) schema)
      MultipleTypes (t :| [t']) ->
        Gen.choice
          [ genValue ranges (over schemaType (const $ SingleType t) schema)
          , genValue ranges (over schemaType (const $ SingleType t') schema)
          ]
      MultipleTypes (t :| (t' : ts)) ->
        Gen.choice
          [ genValue ranges (over schemaType (const $ SingleType t) schema)
          , genValue ranges (over schemaType (const $ MultipleTypes (t' :| ts)) schema)
          ]
      SingleType NullType -> genNullValue
      SingleType BooleanType -> genBooleanValue

genNullValue :: Gen Aeson.Value
genNullValue = Gen.constant Aeson.Null

genBooleanValue :: Gen Aeson.Value
genBooleanValue = Aeson.Bool <$> Gen.bool

--genNumberValue :: Gen Aeson.Value
