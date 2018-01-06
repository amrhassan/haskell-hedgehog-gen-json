{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
  , sensibleRanges
  ) where

import           Control.Monad.Catch             (MonadThrow, throwM)
import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Text                       as Text
import           Hedgehog
import qualified Hedgehog.Gen.JSON.Constrained   as Constrained
import           Hedgehog.Gen.JSON.JSONSchema    (Schema)
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import qualified Hedgehog.Range                  as Range
import           Protolude

readSchema :: (MonadIO m, MonadThrow m) => FilePath -> m Schema
readSchema fp = do
  bytes <- liftIO $ BS.readFile fp
  case Aeson.eitherDecodeStrict bytes of
    Left err     -> throwM $ DecodingSchemaException $ Text.pack err
    Right schema -> pure schema

genJSON :: Ranges -> Gen ByteString
genJSON ranges = (LBS.toStrict . Aeson.encode) <$> genJSONValue ranges

genJSONValue :: Ranges -> Gen Aeson.Value
genJSONValue = Unconstrained.genValue

genConstrainedJSON :: Ranges -> Schema -> Gen ByteString
genConstrainedJSON ranges schema = (LBS.toStrict . Aeson.encode) <$> genConstrainedJSONValue ranges schema

genConstrainedJSONValue :: Ranges -> Schema -> Gen Aeson.Value
genConstrainedJSONValue = Constrained.genValue

sensibleRanges :: Ranges
sensibleRanges =
  Ranges
  { _arrayRange = ArrayRange $ Range.linear 0 5
  , _stringRange = StringRange $ Range.linear 0 1000
  , _numberRange = NumberRange $ Range.linearFrac (-1000) 1000
  , _integerRange = IntegerRange $ Range.linear (-1000) 1000
  , _objectRange = ObjectRange $ Range.linear 0 5
  }

newtype JSONException =
  DecodingSchemaException Text
  deriving (Show, Eq)

instance Exception JSONException
