{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Hedgehog.Gen.JSON.JSONSchema where

import           Control.Lens        (makeLenses)
import           Control.Monad.Fail
import           Data.Aeson          (withObject, (.:?))
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

newtype AnyConstraintEnum = AnyConstraintEnum
  { unArrayConstraintEnum :: NonEmpty Aeson.Value
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyConstraintConst = AnyConstraintConst
  { unArrayConstraintConst :: Aeson.Value
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintMultipleOf = NumberConstraintMultipleOf
  { unNumberConstraintMultipleOf :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintMaximum = NumberConstraintMaximum
  { unNumberConstraintMaximum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintExclusiveMaximum = NumberConstraintExclusiveMaximum
  { unNumberConstraintExclusiveMaximum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintMinimum = NumberConstraintMinimum
  { unNumberConstraintMinimum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype NumberConstraintExclusiveMinimum = NumberConstraintExclusiveMinimum
  { unNumberConstraintExclusiveMinimum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype StringConstraintPattern = StringConstraintPattern
  { unStringConstraintPattern :: Text
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype StringConstraintFormat = StringConstraintFormat
  { unStringConstraintFormat :: Text
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype StringConstraintMaxLength = StringConstraintMaxLength
  { unStringConstraintMaxLength :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype StringConstraintMinLength = StringConstraintMinLength
  { unStringConstraintMinLength :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance Aeson.FromJSON Schema where
  parseJSON =
    withObject "Schema" $ \obj ->
      Schema <$> obj .:? "type" <*> obj .:? "enum" <*> obj .:? "const" <*> obj .:? "properties" <*> obj .:? "required" <*> obj .:? "multipleOf" <*>
      obj .:? "maximum" <*>
      obj .:? "exclusiveMaximum" <*>
      obj .:? "minimum" <*>
      obj .:? "exclusiveMinimum" <*>
      obj .:? "pattern" <*>
      obj .:? "format" <*>
      obj .:? "maxLength" <*>
      obj .:? "minLength" <*>
      obj .:? "items" <*>
      obj .:? "maxItems" <*>
      obj .:? "minItems" <*>
      obj .:? "uniqueItems" <*>
      obj .:? "$ref" <*>
      obj .:? "definitions" <*>
      obj .:? "anyOf" <*>
      obj .:? "oneOf" <*>
      obj .:? "allOf"

newtype ToplevelSchema = ToplevelSchema {
  unToplevelSchema :: Schema
} deriving (Eq, Show, Generic, Aeson.FromJSON)

newtype ObjectConstraintProperties = ObjectConstraintProperties
  { unObjectConstraintProperties :: HM.HashMap Text Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ObjectConstraintRequired = ObjectConstraintRequired
  { unObjectConstraintRequired :: [Text]
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyConstraintRef = AnyConstraintRef
  { unAnyConstraintRef :: Text
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyConstraintDefinitions = AnyConstraintDefinitions
  { unAnyConstraintDefinitions :: HM.HashMap Text Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyConstraintAllOf = AnyConstraintAllOf
  { unAnyConstraintAllOf :: NonEmpty Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyConstraintOneOf = AnyConstraintOneOf
  { unAnyConstraintOneOf :: NonEmpty Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype AnyConstraintAnyOf = AnyConstraintAnyOf
  { unAnyConstraintAnyOf :: NonEmpty Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ArrayConstraintItems = ArrayConstraintItems
  { unArrayConstraintItems :: Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ArrayConstraintMaxItems = ArrayConstraintMaxItems
  { unArrayConstraintMaxItems :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ArrayConstraintMinItems = ArrayConstraintMinItems
  { unArrayConstraintMinItems :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

newtype ArrayConstraintUniqueItems = ArrayConstraintUniqueItems
  { unArrayConstraintUniqueItems :: Bool
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

data Schema = Schema
  { _schemaType             :: Maybe AnyConstraintType
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
  , _schemaFormat           :: Maybe StringConstraintFormat
  , _schemaMaxLength        :: Maybe StringConstraintMaxLength
  , _schemaMinLength        :: Maybe StringConstraintMinLength
  , _schemaItems            :: Maybe ArrayConstraintItems
  , _schemaMaxItems         :: Maybe ArrayConstraintMaxItems
  , _schemaMinItems         :: Maybe ArrayConstraintMinItems
  , _schemaUniqueItems      :: Maybe ArrayConstraintUniqueItems
  , _schemaRef              :: Maybe AnyConstraintRef
  , _schemaDefinitions      :: Maybe AnyConstraintDefinitions
  , _schemaAnyOf            :: Maybe AnyConstraintAnyOf
  , _schemaOneOf            :: Maybe AnyConstraintOneOf
  , _schemaAllOf            :: Maybe AnyConstraintAllOf
  } deriving (Generic, Eq, Show)

emptySchema :: Schema
emptySchema =
  Schema
    { _schemaType = Nothing
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
    , _schemaFormat = Nothing
    , _schemaMinLength = Nothing
    , _schemaMaxLength = Nothing
    , _schemaItems = Nothing
    , _schemaMinItems = Nothing
    , _schemaMaxItems = Nothing
    , _schemaUniqueItems = Nothing
    , _schemaRef = Nothing
    , _schemaDefinitions = Nothing
    , _schemaAnyOf = Nothing
    , _schemaOneOf = Nothing
    , _schemaAllOf = Nothing
    }

makeLenses ''Schema

read :: FilePath -> IO (Either Text Schema)
read fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (Aeson.decodeStrict bytes)
