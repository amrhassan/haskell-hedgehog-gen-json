{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hedgehog.Gen.JSON.JSONSchema where

import           Control.Lens        (makeLenses)
import           Control.Monad.Fail
import           Data.Aeson          (withObject, withText, (.:), (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import           Protolude

data PrimitiveType
  = NullType
  | BooleanType
  | ObjectType
  | ArrayType
  | NumberType
  | StringType
  | IntegerType
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Aeson.FromJSON PrimitiveType where
  parseJSON (Aeson.String t) =
    case Text.toLower t of
      "null" -> pure NullType
      "bool" -> pure BooleanType
      "array" -> pure ArrayType
      "integer" -> pure IntegerType
      "number" -> pure NumberType
      "string" -> pure StringType
      "object" -> pure ObjectType
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
  AnyKeywordEnum (NonEmpty Aeson.Value) -- TODO: should be a NonEmptySet
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyKeywordConst =
  AnyKeywordConst Aeson.Value
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ObjectKeywordProperties =
  ObjectKeywordProperties (HM.HashMap Text Schema)
  deriving (Generic, Eq, Show)

instance Aeson.FromJSON ObjectKeywordProperties

newtype ObjectKeywordRequired =
  ObjectKeywordRequired (Set Text)
  deriving (Generic, Eq, Show, Aeson.FromJSON)

data Schema = Schema
  { _schemaType :: AnyKeywordType
  , _schemaEnum       :: Maybe AnyKeywordEnum
  , _schemaConst      :: Maybe AnyKeywordConst
  , _schemaProperties :: Maybe ObjectKeywordProperties
  , _schemaRequired   :: Maybe ObjectKeywordRequired
  } deriving (Generic, Eq, Show)

objectSchema :: Schema
objectSchema =
  Schema
  {_schemaType = SingleType ObjectType, _schemaEnum = Nothing, _schemaConst = Nothing, _schemaRequired = Nothing, _schemaProperties = Nothing}

makeLenses ''Schema

instance Aeson.FromJSON Schema where
  parseJSON =
    withObject "Schema" $ \obj ->
      Schema <$> obj .: "type" <*> obj .:? "enum" <*> obj .:? "const" <*> obj .:? "properties" <*> obj .:? "required"

read :: FilePath -> IO (Either Text Schema)
read fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (Aeson.decodeStrict bytes)
