{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
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
  ) where

import           Control.Monad.Catch             (MonadThrow, throwM)
import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import           Hedgehog
import qualified Hedgehog.Gen.JSON.Constrained   as Constrained
import           Hedgehog.Gen.JSON.JSONSchema    (Schema)
import           Hedgehog.Gen.JSON.Ranges
import qualified Hedgehog.Gen.JSON.Unconstrained as Unconstrained
import qualified Prelude
import           Protolude

readSchema :: (MonadIO m, MonadThrow m) => FilePath -> m Schema
readSchema fp = do
  bytes <- liftIO $ BS.readFile fp
  liftEither $ Aeson.eitherDecodeStrict bytes

genJSON :: Ranges -> Gen ByteString
genJSON ranges = (LBS.toStrict . Aeson.encode) <$> genJSONValue ranges

genJSONValue :: Ranges -> Gen Aeson.Value
genJSONValue = Unconstrained.genValue

genConstrainedJSON :: Ranges -> Schema -> Gen ByteString
genConstrainedJSON ranges schema = (LBS.toStrict . Aeson.encode) <$> genConstrainedJSONValue ranges schema

genConstrainedJSONValue :: Ranges -> Schema -> Gen Aeson.Value
genConstrainedJSONValue = Constrained.genValue

liftEither :: (Exception e, MonadThrow m) => Either e a -> m a
liftEither (Left e)  = throwM e
liftEither (Right a) = pure a

instance Exception Prelude.String
