{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Gen.JSON
  ( genJSON
  , genJSONValue
  , genConstrainedJSON
  , genConstrainedJSONValue
  , Schema
  , readSchema
  , Ranges(..)
  , NumberRange(..)
  , StringRange(..)
  , ArrayRange(..)
  , ObjectRange(..)
  ) where

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Scientific                 as Scientific
import qualified Data.Vector                     as Vector
import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Gen.JSON.Constrained   as Constrained
import           Hedgehog.Gen.JSON.JSONSchema    (Schema)
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import           Protolude

readSchema :: FilePath -> IO (Either Text Schema)
readSchema fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (Aeson.decodeStrict bytes)

genJSON :: Ranges -> Gen ByteString
genJSON ranges = (LBS.toStrict . Aeson.encode) <$> genJSONValue ranges

genJSONValue :: Ranges -> Gen Aeson.Value
genJSONValue = Unconstrained.genValue

genConstrainedJSON :: Ranges -> Schema -> Gen ByteString
genConstrainedJSON ranges schema = (LBS.toStrict . Aeson.encode) <$> genConstrainedJSONValue ranges schema

genConstrainedJSONValue :: Ranges -> Schema -> Gen Aeson.Value
genConstrainedJSONValue = Constrained.genValue
