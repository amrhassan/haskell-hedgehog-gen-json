{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Gen.JSON
  ( genJSON
  , genConstrainedJSON
  , Ranges(..)
  , NumberRange(..)
  , StringRange(..)
  , ArrayRange(..)
  , ObjectRange(..)
  ) where

import qualified Data.Aeson                      as A
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Scientific                 as Scientific
import qualified Data.Vector                     as Vector
import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Gen.JSON.Constrained   as Constrained
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import qualified JSONSchema.Draft4               as D4
import           Protolude

readD4Schema :: FilePath -> IO (Either Text D4.Schema)
readD4Schema fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (A.decodeStrict bytes)

genJSON :: Ranges -> Gen ByteString
genJSON ranges = (LBS.toStrict . A.encode) <$> Unconstrained.genValue ranges

genConstrainedJSON :: Ranges -> D4.Schema -> Gen ByteString
genConstrainedJSON ranges schema = (LBS.toStrict . A.encode) <$> Constrained.genValue ranges schema
