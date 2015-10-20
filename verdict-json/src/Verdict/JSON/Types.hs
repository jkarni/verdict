{-# LANGUAGE OverloadedStrings #-}
module Verdict.JSON.Types
    ( JsonConstraint(..)
    , SchemaType(..)
    , JSONKey
    , ValidJSONKey
    , JsonSpec(..)
    ) where

import           Data.Aeson
import qualified Data.Map  as Map
import qualified Data.Text as Text
import           Data.Vector (fromList)
import           GHC.Generics (Generic)
import           Verdict

data JsonConstraint a
    = Minimum Int
    | Maximum Int
    | MaxLength Int
    | MinLength Int
    | OtherError a
    deriving (Eq, Show, Read, Functor, Generic)

data Regex = Regex
  deriving (Eq, Show, Read, Generic)

data NumericT = JSONInteger | JSONNumeric
  deriving (Eq, Show, Read, Generic)

data AnySchema = ObjectS ObjectSchema
               | NumericS NumericSchema
               | ArrayS ArraySchema
               | StringS StringSchema
  deriving (Eq, Show, Read, Generic)

instance ToJSON AnySchema where
    toJSON (ObjectS os) = toJSON os

data Either' a b = Left' a | Right' b
    deriving (Eq, Show, Read, Functor, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
    toJSON (Left' a)  = toJSON a
    toJSON (Right' b) = toJSON b

data ObjectSchema = ObjectSchema
    { properties           :: Map.Map Text.Text (Required, AnySchema)
    , additionalProperties :: Either' Bool AnySchema
    , patternProperties    :: Map.Map Text.Text AnySchema
    } deriving (Eq, Show, Read, Generic)

instance ToJSON ObjectSchema where
    toJSON os = object [
        "properties"           .= toJSON (snd <$> properties os)
      , "required"             .= Array (String <$> fromList reqs)
      , "additionalProperties" .= toJSON (additionalProperties os)
      , "patternProperties"    .= toJSON (patternProperties os)
      ]
      where reqs = Map.keys $ Map.filter ((== Required) . fst) $ properties os

data NumericSchema = NumericSchema
    { multipleOf :: Maybe Int
    , maximum    :: Maybe Int
    , minimum    :: Maybe Int
    } deriving (Eq, Show, Read, Generic)

data ArraySchema = ArraySchema
    { items           :: [AnySchema]
    , additionalItems :: Either Bool AnySchema
    } deriving (Eq, Show, Read, Generic)

data StringSchema = StringSchema
    { maxLength :: Maybe Int
    , minLength :: Maybe Int
    , pattern   :: Maybe Regex
    } deriving (Eq, Show, Read, Generic)

data Metadata = Metadata
    { title       :: Maybe Text.Text
    , description :: Maybe Text.Text
    }




data SchemaVersion = Draft4
  deriving (Eq, Show, Read, Generic)

data Required = Required | NotRequired
  deriving (Eq, Show, Read, Generic)

{-data JsonProperty a-}
    {-= JsonProperty { constraints :: [JsonConstraint a]-}
                   {-, type'       :: SchemaType-}
                   {-, description :: Maybe NonEmptyText-}
                   {-} deriving (Eq, Show, Read, Generic)-}

data SchemaType
    = StringT
    | NumberT
    | IntegerT
    | BooleanT
    | ObjectT
    | ArrayT SchemaType
    | NullT
    | AnyT
    deriving (Eq, Show, Read, Generic)


data ValidJSONKey

instance HaskVerdict ValidJSONKey Text.Text where
    haskVerdict _ = check (\x -> Text.foldr go Continue x /= Bad)
                          "Should have escaped single quotes"
     where
       go _ Bad        = Bad
       go '\\' _       = Escaped
       go '\'' Escaped = Continue
       go '\'' _       = Bad
       go _ _          = Continue

data C = Continue | Escaped | Bad
    deriving (Eq, Show, Read)

type JSONKey = Validated ValidJSONKey Text.Text
newtype JsonSpec = JsonSpec
  { unspec :: Map.Map JSONKey (Either [JsonConstraint Text.Text] JsonSpec) }
  deriving (Eq, Show, Read)

instance ToJSON JsonSpec where
    toJSON = undefined
