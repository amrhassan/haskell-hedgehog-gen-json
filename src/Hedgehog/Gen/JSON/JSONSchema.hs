{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances #-}

module Hedgehog.Gen.JSON.JSONSchema where

import           Control.Lens        (makeLenses)
import           Control.Monad.Fail
import           Data.Aeson          (withObject, (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as List
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Scientific     (Scientific)
import           Data.Semigroup
import qualified Data.Text           as Text
import           Protolude            hiding ((<>))

class CombineAnd a where
  (*&*) :: a -> a -> Either Text a

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

instance CombineAnd AnyConstraintType where
  (SingleType x) *&* (SingleType y) = MultipleTypes (NonEmpty.fromList [x]) *&* MultipleTypes (NonEmpty.fromList [y])
  (SingleType x) *&* (MultipleTypes ys) = MultipleTypes (NonEmpty.fromList [x]) *&* MultipleTypes ys
  (MultipleTypes xs) *&* (SingleType y) = MultipleTypes xs *&* MultipleTypes (NonEmpty.fromList [y])
  (MultipleTypes xs) *&* (MultipleTypes ys) =
    let zs = nonEmpty $ (toList xs) `List.intersect` (toList ys) in
      maybeToRight "Intersection of types is an empty list" (MultipleTypes <$> zs)

instance Aeson.FromJSON AnyConstraintType where
  parseJSON str@(Aeson.String _) = SingleType <$> Aeson.parseJSON str
  parseJSON (Aeson.Array ts) = (MultipleTypes . NonEmpty.fromList . toList) <$> traverse Aeson.parseJSON ts
  parseJSON _ = fail "type must be either a string or an array of strings"

newtype AnyConstraintEnum = AnyConstraintEnum
  { unArrayConstraintEnum :: NonEmpty Aeson.Value
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd AnyConstraintEnum where
  (AnyConstraintEnum x) *&* (AnyConstraintEnum y) =
    maybeToEither "intersection of `enum` is an empty list" (AnyConstraintEnum <$> nonEmpty (toList x `List.intersect` toList y))

newtype AnyConstraintConst = AnyConstraintConst
  { unArrayConstraintConst :: Aeson.Value
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd AnyConstraintConst where
  (AnyConstraintConst x) *&* (AnyConstraintConst y)
    | x == y    = Right (AnyConstraintConst x)
    | otherwise = Left "can't and-combine unequal `const`"

newtype NumberConstraintMultipleOf = NumberConstraintMultipleOf
  { unNumberConstraintMultipleOf :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd NumberConstraintMultipleOf where
   (NumberConstraintMultipleOf x) *&* (NumberConstraintMultipleOf y) = Right $ NumberConstraintMultipleOf $ x * y

newtype NumberConstraintMaximum = NumberConstraintMaximum
  { unNumberConstraintMaximum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd NumberConstraintMaximum where
  (NumberConstraintMaximum x) *&* (NumberConstraintMaximum y) =
    Right $ NumberConstraintMaximum $ min x y

newtype NumberConstraintExclusiveMaximum = NumberConstraintExclusiveMaximum
  { unNumberConstraintExclusiveMaximum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd NumberConstraintExclusiveMaximum where
  (NumberConstraintExclusiveMaximum x) *&* (NumberConstraintExclusiveMaximum y) =
    Right $ NumberConstraintExclusiveMaximum $ min x y

newtype NumberConstraintMinimum = NumberConstraintMinimum
  { unNumberConstraintMinimum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd NumberConstraintMinimum where
  (NumberConstraintMinimum x) *&* (NumberConstraintMinimum y) =
    Right $ NumberConstraintMinimum $ max x y

newtype NumberConstraintExclusiveMinimum = NumberConstraintExclusiveMinimum
  { unNumberConstraintExclusiveMinimum :: Scientific
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd NumberConstraintExclusiveMinimum where
  (NumberConstraintExclusiveMinimum x) *&* (NumberConstraintExclusiveMinimum y) =
    Right $ NumberConstraintExclusiveMinimum $ max x y

newtype StringConstraintPattern = StringConstraintPattern
  { unStringConstraintPattern :: Text
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd StringConstraintPattern where
  _ *&* _ = Left "can't and-combine `pattern`"

newtype StringConstraintFormat = StringConstraintFormat
  { unStringConstraintFormat :: Text
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd StringConstraintFormat where
  _ *&* _ = Left "can't and-combine `format`"

newtype StringConstraintMaxLength = StringConstraintMaxLength
  { unStringConstraintMaxLength :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd StringConstraintMaxLength where
  (StringConstraintMaxLength x) *&* (StringConstraintMaxLength y) =
    Right $ StringConstraintMaxLength $ min x y

newtype StringConstraintMinLength = StringConstraintMinLength
  { unStringConstraintMinLength :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd StringConstraintMinLength where
  (StringConstraintMinLength x) *&* (StringConstraintMinLength y) =
    Right $ StringConstraintMinLength $ min x y

instance Aeson.FromJSON Schema where
  parseJSON =
    withObject "Schema" $ \obj ->
      Schema <$>
      obj .:? "type" <*>
      obj .:? "enum" <*>
      obj .:? "const" <*>
      obj .:? "properties" <*>
      obj .:? "required" <*>
      obj .:? "multipleOf" <*>
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

newtype ToplevelSchema = ToplevelSchema
  { unToplevelSchema :: Schema
  } deriving (Eq, Show, Generic, Aeson.FromJSON)

newtype ObjectConstraintProperties = ObjectConstraintProperties
  { unObjectConstraintProperties :: HM.HashMap Text Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd ObjectConstraintProperties where
  (ObjectConstraintProperties x) *&* (ObjectConstraintProperties y)
    | HM.null (x `HM.intersection` y) = Right $ ObjectConstraintProperties $ x <> y
    | otherwise                       = Left "can't and-combine overlapping properties"

newtype ObjectConstraintRequired = ObjectConstraintRequired
  { unObjectConstraintRequired :: [Text]
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd ObjectConstraintRequired where
  (ObjectConstraintRequired x) *&* (ObjectConstraintRequired y) =
    Right $ ObjectConstraintRequired $ x <> y

newtype AnyConstraintRef = AnyConstraintRef
  { unAnyConstraintRef :: Text
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd AnyConstraintRef where 
  _ *&* _ = Left "can't and-combine `$ref`"

newtype AnyConstraintDefinitions = AnyConstraintDefinitions
  { unAnyConstraintDefinitions :: HM.HashMap Text Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd AnyConstraintDefinitions where
  (AnyConstraintDefinitions x) *&* (AnyConstraintDefinitions y)
    | HM.null (x `HM.intersection` y) = Right $ AnyConstraintDefinitions $ x <> y
    | otherwise                       = Left "can't and-combine overlapping definitions"

newtype AnyConstraintAllOf = AnyConstraintAllOf
  { unAnyConstraintAllOf :: NonEmpty Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd AnyConstraintAllOf where
  (AnyConstraintAllOf x) *&* (AnyConstraintAllOf y) =
    Right $ AnyConstraintAllOf (x <> y)

newtype AnyConstraintOneOf = AnyConstraintOneOf
  { unAnyConstraintOneOf :: NonEmpty Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd AnyConstraintOneOf where
  (AnyConstraintOneOf x) *&* (AnyConstraintOneOf y) =
    maybeToEither "intersection of `oneOf` is an empty list" (AnyConstraintOneOf <$> nonEmpty (List.intersect (toList x) (toList y)))

newtype AnyConstraintAnyOf = AnyConstraintAnyOf
  { unAnyConstraintAnyOf :: NonEmpty Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd AnyConstraintAnyOf where
  (AnyConstraintAnyOf x) *&* (AnyConstraintAnyOf y) =
    maybeToEither "intersection of `anyOf` is an empty list" (AnyConstraintAnyOf <$> nonEmpty (List.intersect (toList x) (toList y)))

newtype ArrayConstraintItems = ArrayConstraintItems
  { unArrayConstraintItems :: Schema
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd ArrayConstraintItems where
  (ArrayConstraintItems x) *&* (ArrayConstraintItems y) =
    ArrayConstraintItems <$> x *&* y

newtype ArrayConstraintMaxItems = ArrayConstraintMaxItems
  { unArrayConstraintMaxItems :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd ArrayConstraintMaxItems where
  (ArrayConstraintMaxItems x) *&* (ArrayConstraintMaxItems y) =
    Right $ ArrayConstraintMaxItems $ min x y

newtype ArrayConstraintMinItems = ArrayConstraintMinItems
  { unArrayConstraintMinItems :: Int
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd ArrayConstraintMinItems where
  (ArrayConstraintMinItems x) *&* (ArrayConstraintMinItems y) =
    Right $ ArrayConstraintMinItems $ max x y

newtype ArrayConstraintUniqueItems = ArrayConstraintUniqueItems
  { unArrayConstraintUniqueItems :: Bool
  } deriving (Generic, Eq, Show, Aeson.FromJSON)

instance CombineAnd ArrayConstraintUniqueItems where
  (ArrayConstraintUniqueItems x) *&* (ArrayConstraintUniqueItems y)
    | x == y = Right $ ArrayConstraintUniqueItems x
    | otherwise = Left "can't and-combine different `unique` constraint"

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

makeLenses ''Schema

instance CombineAnd a => CombineAnd (Maybe a) where
  Nothing *&* Nothing = Right Nothing
  (Just x) *&* Nothing = Right $ Just x
  Nothing *&* (Just x) = Right $ Just x
  (Just x) *&* (Just y) = Just <$> x *&* y

instance CombineAnd Schema where
  x *&* y = Schema <$> 
    (_schemaType x *&* _schemaType y) <*>
    (_schemaEnum x *&* _schemaEnum y) <*>
    (_schemaConst x *&* _schemaConst y) <*>
    (_schemaProperties x *&* _schemaProperties y) <*>
    (_schemaRequired x *&* _schemaRequired y) <*>
    (_schemaMultipleOf x *&* _schemaMultipleOf y) <*>
    (_schemaMaximum x *&* _schemaMaximum y) <*>
    (_schemaExclusiveMaximum x *&* _schemaExclusiveMaximum y) <*>
    (_schemaMinimum x *&* _schemaMinimum y) <*>
    (_schemaExclusiveMinimum x *&* _schemaExclusiveMinimum y) <*>
    (_schemaPattern x *&* _schemaPattern y) <*>
    (_schemaFormat x *&* _schemaFormat y) <*>
    (_schemaMaxLength x *&* _schemaMaxLength y) <*>
    (_schemaMinLength x *&* _schemaMinLength y) <*>
    (_schemaItems x *&* _schemaItems y) <*>
    (_schemaMaxItems x *&* _schemaMaxItems y) <*>
    (_schemaMinItems x *&* _schemaMinItems y) <*>
    (_schemaUniqueItems x *&* _schemaUniqueItems y) <*>
    (_schemaRef x *&* _schemaRef y) <*>
    (_schemaDefinitions x *&* _schemaDefinitions y) <*>
    (_schemaAnyOf x *&* _schemaAnyOf y) <*>
    (_schemaOneOf x *&* _schemaOneOf y) <*>
    (_schemaAllOf x *&* _schemaAllOf y)


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


read :: FilePath -> IO (Either Text Schema)
read fp = do
  bytes <- BS.readFile fp
  pure $ maybeToEither "failed to decode JSON Schema" (Aeson.decodeStrict bytes)
