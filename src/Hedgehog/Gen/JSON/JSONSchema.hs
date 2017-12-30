{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hedgehog.Gen.JSON.JSONSchema where

import           Control.Lens        (makeLenses)
import           Control.Monad.Fail
import           Data.Aeson          (withObject, (.:), (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Scientific     (Scientific)
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

data AnyConstraintType
  = SingleType PrimitiveType
  | MultipleTypes (NonEmpty PrimitiveType)
  deriving (Eq, Show)

instance Aeson.FromJSON AnyConstraintType where
  parseJSON str@(Aeson.String _) = SingleType <$> Aeson.parseJSON str
  parseJSON (Aeson.Array ts) = (MultipleTypes . NonEmpty.fromList . toList) <$> traverse Aeson.parseJSON ts
  parseJSON _ = fail "type must be either a string or an array of strings"

newtype AnyConstraintEnum =
  AnyConstraintEnum (NonEmpty Aeson.Value)
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyConstraintConst =
  AnyConstraintConst Aeson.Value
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintMultipleOf =
  NumberConstraintMultipleOf Scientific
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintMaximum =
  NumberConstraintMaximum Scientific
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintExclusiveMaximum =
  NumberConstraintExclusiveMaximum Scientific
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintMinimum =
  NumberConstraintMinimum Scientific
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintExclusiveMinimum =
  NumberConstraintExclusiveMinimum Scientific
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype StringConstraintPattern =
  StringConstraintPattern Text
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype StringConstraintMaxLength =
  StringConstraintMaxLength Int
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype StringConstraintMinLength =
  StringConstraintMinLength Int
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ObjectConstraintProperties =
  ObjectConstraintProperties (HM.HashMap Text Schema)
  deriving (Generic, Eq, Show)

instance Aeson.FromJSON ObjectConstraintProperties

instance Aeson.FromJSON ArrayConstraintItems

newtype ObjectConstraintRequired =
  ObjectConstraintRequired [Text]
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ArrayConstraintItems =
  ArrayConstraintItems Schema
  deriving (Generic, Eq, Show)

newtype ArrayConstraintMaxItems =
  ArrayConstraintMaxItems Int
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ArrayConstraintMinItems =
  ArrayConstraintMinItems Int
  deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ArrayConstraintUniqueItems =
  ArrayConstraintUniqueItems Bool
  deriving (Generic, Eq, Show, Aeson.FromJSON)

data Schema = Schema
  { _schemaType             :: AnyConstraintType
  , _schemaEnum             :: Maybe AnyConstraintEnum
  , _schemaConst            :: Maybe AnyConstraintConst
  , _schemaProperties       :: Maybe ObjectConstraintProperties
  , _schemaRequired         :: Maybe ObjectConstraintRequired
  , _schemaMultipleOf       :: Maybe NumberConstraintMultipleOf
  , _schemaMaximum          :: Maybe NumberConstraintMaximum
  , _schemaExclusiveMaximum :: Maybe NumberConstraintExclusiveMaximum
  , _schemaMinimum          :: Maybe NumberConstraintMinimum
  , _schemaExclusiveMinimum :: Maybe NumberConstraintExclusiveMinimum
  , _schemaPattern          :: Maybe StringConstraintPattern
  , _schemaMaxLength        :: Maybe StringConstraintMaxLength
  , _schemaMinLength        :: Maybe StringConstraintMinLength
  , _schemaItems            :: Maybe ArrayConstraintItems
  , _schemaMaxItems         :: Maybe ArrayConstraintMaxItems
  , _schemaMinItems         :: Maybe ArrayConstraintMinItems
  , _schemaUniqueItems      :: Maybe ArrayConstraintUniqueItems
  } deriving (Generic, Eq, Show)

nullSchema :: Schema
nullSchema =
  Schema
  { _schemaType = SingleType NullType
  , _schemaEnum = Nothing
  , _schemaConst = Nothing
  , _schemaRequired = Nothing
  , _schemaProperties = Nothing
  , _schemaMultipleOf = Nothing
  , _schemaMaximum = Nothing
  , _schemaMinimum = Nothing
  , _schemaExclusiveMaximum = Nothing
  , _schemaExclusiveMinimum = Nothing
  , _schemaPattern = Nothing
  , _schemaMinLength = Nothing
  , _schemaMaxLength = Nothing
  , _schemaItems = Nothing
  , _schemaMinItems = Nothing
  , _schemaMaxItems = Nothing
  , _schemaUniqueItems = Nothing
  }

booleanSchema :: Schema
booleanSchema =
  Schema
  { _schemaType = SingleType BooleanType
  , _schemaEnum = Nothing
  , _schemaConst = Nothing
  , _schemaRequired = Nothing
  , _schemaProperties = Nothing
  , _schemaMultipleOf = Nothing
  , _schemaMaximum = Nothing
  , _schemaMinimum = Nothing
  , _schemaExclusiveMaximum = Nothing
  , _schemaExclusiveMinimum = Nothing
  , _schemaPattern = Nothing
  , _schemaMinLength = Nothing
  , _schemaMaxLength = Nothing
  , _schemaItems = Nothing
  , _schemaMinItems = Nothing
  , _schemaMaxItems = Nothing
  , _schemaUniqueItems = Nothing
  }

objectSchema :: Schema
objectSchema =
  Schema
  { _schemaType = SingleType ObjectType
  , _schemaEnum = Nothing
  , _schemaConst = Nothing
  , _schemaRequired = Nothing
  , _schemaProperties = Nothing
  , _schemaMultipleOf = Nothing
  , _schemaMaximum = Nothing
  , _schemaMinimum = Nothing
  , _schemaExclusiveMaximum = Nothing
  , _schemaExclusiveMinimum = Nothing
  , _schemaPattern = Nothing
  , _schemaMinLength = Nothing
  , _schemaMaxLength = Nothing
  , _schemaItems = Nothing
  , _schemaMinItems = Nothing
  , _schemaMaxItems = Nothing
  , _schemaUniqueItems = Nothing
  }

arraySchema :: Schema
arraySchema =
  Schema
  { _schemaType = SingleType ArrayType
  , _schemaEnum = Nothing
  , _schemaConst = Nothing
  , _schemaRequired = Nothing
  , _schemaProperties = Nothing
  , _schemaMultipleOf = Nothing
  , _schemaMaximum = Nothing
  , _schemaMinimum = Nothing
  , _schemaExclusiveMaximum = Nothing
  , _schemaExclusiveMinimum = Nothing
  , _schemaPattern = Nothing
  , _schemaMinLength = Nothing
  , _schemaMaxLength = Nothing
  , _schemaItems = Nothing
  , _schemaMinItems = Nothing
  , _schemaMaxItems = Nothing
  , _schemaUniqueItems = Nothing
  }

numberSchema :: Schema
numberSchema =
  Schema
  { _schemaType = SingleType NumberType
  , _schemaEnum = Nothing
  , _schemaConst = Nothing
  , _schemaRequired = Nothing
  , _schemaProperties = Nothing
  , _schemaMultipleOf = Nothing
  , _schemaMaximum = Nothing
  , _schemaMinimum = Nothing
  , _schemaExclusiveMaximum = Nothing
  , _schemaExclusiveMinimum = Nothing
  , _schemaPattern = Nothing
  , _schemaMinLength = Nothing
  , _schemaMaxLength = Nothing
  , _schemaItems = Nothing
  , _schemaMinItems = Nothing
  , _schemaMaxItems = Nothing
  , _schemaUniqueItems = Nothing
  }

integerSchema :: Schema
integerSchema =
  Schema
  { _schemaType = SingleType IntegerType
  , _schemaEnum = Nothing
  , _schemaConst = Nothing
  , _schemaRequired = Nothing
  , _schemaProperties = Nothing
  , _schemaMultipleOf = Nothing
  , _schemaMaximum = Nothing
  , _schemaMinimum = Nothing
  , _schemaExclusiveMaximum = Nothing
  , _schemaExclusiveMinimum = Nothing
  , _schemaPattern = Nothing
  , _schemaMinLength = Nothing
  , _schemaMaxLength = Nothing
  , _schemaItems = Nothing
  , _schemaMinItems = Nothing
  , _schemaMaxItems = Nothing
  , _schemaUniqueItems = Nothing
  }

stringSchema :: Schema
stringSchema =
  Schema
  { _schemaType = SingleType StringType
  , _schemaEnum = Nothing
  , _schemaConst = Nothing
  , _schemaRequired = Nothing
  , _schemaProperties = Nothing
  , _schemaMultipleOf = Nothing
  , _schemaMaximum = Nothing
  , _schemaMinimum = Nothing
  , _schemaExclusiveMaximum = Nothing
  , _schemaExclusiveMinimum = Nothing
  , _schemaPattern = Nothing
  , _schemaMinLength = Nothing
  , _schemaMaxLength = Nothing
  , _schemaItems = Nothing
  , _schemaMinItems = Nothing
  , _schemaMaxItems = Nothing
  , _schemaUniqueItems = Nothing
  }

makeLenses ''Schema

instance Aeson.FromJSON Schema where
  parseJSON =
    withObject "Schema" $ \obj ->
      Schema <$> obj .: "type" <*> obj .:? "enum" <*> obj .:? "const" <*> obj .:? "properties" <*> obj .:? "required" <*>
      obj .:? "multipleOf" <*>
      obj .:? "maximum" <*>
      obj .:? "exclusiveMaximum" <*>
      obj .:? "minimum" <*>
      obj .:? "exclusiveMinimum" <*>
      obj .:? "pattern" <*>
      obj .:? "maxLength" <*>
      obj .:? "minLength" <*>
      obj .:? "items" <*>
      obj .:? "maxItems" <*>
      obj .:? "minItems" <*>
      obj .:? "uniqueItems"

read :: FilePath -> IO (Either Text Schema)
read fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (Aeson.decodeStrict bytes)
