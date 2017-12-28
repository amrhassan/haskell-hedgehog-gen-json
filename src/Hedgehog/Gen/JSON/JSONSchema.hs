{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hedgehog.Gen.JSON.JSONSchema where

import           Control.Lens       (makeLenses)
import           Control.Monad.Fail
import           Data.Aeson         (withObject, withText, (.:))
import qualified Data.Aeson         as Aeson
import qualified Data.ByteString    as BS
import qualified Data.Set           as Set
import qualified Data.Text          as Text
import           Protolude

data PrimitiveType
  = NullType
  | BooleanType
  | ObjectType
  | ArrayType
  | NumberType
  | StringType
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON PrimitiveType where
  parseJSON (Aeson.String t) =
    case Text.toLower t of
      "null" -> pure NullType
      "bool" -> pure BooleanType
      "array" -> pure ArrayType
      "number" -> pure NumberType
      "string" -> pure StringType
      _ -> fail "Primitive type is not one of (null, bool, array, number, string)"
  parseJSON _ = fail "type is not a JSON String"

data AnyKeywordType
  = SingleType PrimitiveType
  | MultipleTypes (Set PrimitiveType)
  deriving (Eq, Show)

instance Aeson.FromJSON AnyKeywordType where
  parseJSON str@(Aeson.String _) = SingleType <$> Aeson.parseJSON str
  parseJSON arr@(Aeson.Array ts) = (MultipleTypes . Set.fromList . toList) <$> traverse Aeson.parseJSON ts
  parseJSON _ = fail "type must be either a string or an array of strings"

newtype AnyKeywordEnum =
  KeywordEnum (NonEmpty Aeson.Value)
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyKeywordConst =
  KeywordConst Aeson.Value
  deriving (Generic, Eq, Show, Aeson.FromJSON)

data Schema = Schema
  { _schemaType :: AnyKeywordType
  , _enum       :: Maybe AnyKeywordEnum
  , _const      :: Maybe AnyKeywordConst
  } deriving (Generic, Eq, Show)

makeLenses ''Schema

instance Aeson.FromJSON Schema where
  parseJSON = withObject "Schema" $ \obj -> Schema <$> obj .: "schema" <*> obj .: "enum" <*> obj .: "const"

readSchema :: FilePath -> IO (Either Text Schema)
readSchema fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (Aeson.decodeStrict bytes)
