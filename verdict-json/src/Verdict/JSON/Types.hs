{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Verdict.JSON.Types where

import           Data.Aeson
import qualified Data.Map  as Map
import Data.Maybe
import           Data.Monoid
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
               {-| ArrayS ArraySchema-}
               | StringS StringSchema
  deriving (Eq, Show, Read, Generic)

instance Monoid AnySchema where
    mempty = ObjectS mempty
    (ObjectS a) `mappend` (ObjectS b) = ObjectS (a <> b)
    (NumericS a) `mappend` (NumericS b) = NumericS (a <> b)
    (StringS a) `mappend` (StringS b) = StringS (a <> b)
    _ `mappend` _ = error "must be same constructor"


instance ToJSON AnySchema where
    toJSON (ObjectS os) = toJSON os
    toJSON (NumericS ns) = toJSON ns
    toJSON (StringS ss) = toJSON ss

data Either' a b = Left' a | Right' b
    deriving (Eq, Show, Read, Functor, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
    toJSON (Left' a)  = toJSON a
    toJSON (Right' b) = toJSON b

data ObjectSchema = ObjectSchema
    { properties           :: Map.Map Text.Text (Required, AnySchema)
    , additionalProperties :: ()
    , patternProperties    :: Map.Map Text.Text AnySchema
    } deriving (Eq, Show, Read, Generic)

instance Monoid ObjectSchema where
    mempty = ObjectSchema mempty mempty mempty
    a `mappend` b = ObjectSchema
        { properties           = properties a <> properties b
        , additionalProperties = mempty
        , patternProperties    = patternProperties a <> patternProperties b
        }


instance ToJSON ObjectSchema where
    toJSON os = object [
        "properties"           .= toJSON (snd <$> properties os)
      , "required"             .= Array (String <$> fromList reqs)
      , "additionalProperties" .= toJSON (additionalProperties os)
      , "patternProperties"    .= toJSON (patternProperties os)
      ]
      where reqs = Map.keys $ Map.filter ((== Required) . fst) $ properties os

data NumericSchema = NumericSchema
    { multipleOf  :: [Int]
    , maximum'    :: Maybe Max
    , minimum'    :: Maybe Min
    } deriving (Eq, Show, Read, Generic)

instance Monoid NumericSchema where
    mempty = NumericSchema mempty mempty mempty
    a `mappend` b = NumericSchema { multipleOf = multipleOf a <> multipleOf b
                                 , maximum' = maximum' a <> maximum' b
                                 , minimum' = minimum' a <> minimum' b
                                 }

instance ToJSON NumericSchema where
    toJSON ns = object [
        "multipleOf" .= toJSON (multipleOf ns)
      , "maximum"    .= toJSON (unMax $ fromMaybe maxBound $ maximum' ns)
      , "minimum"    .= toJSON (unMin $ fromMaybe minBound $ minimum' ns)
      ]

newtype Max = Max { unMax :: Int}
    deriving (Eq, Show, Bounded, Ord, Read, Generic)

instance Monoid Max where
    mempty = minBound
    mappend = max

newtype Min = Min { unMin :: Int }
    deriving (Eq, Show, Bounded, Ord, Read, Generic)

instance Monoid Min where
    mempty = maxBound
    mappend = min


data ArraySchema = ArraySchema
    { items           :: [AnySchema]
    , additionalItems :: Either Bool AnySchema
    } deriving (Eq, Show, Read, Generic)

data StringSchema = StringSchema
    { maxLength :: Maybe Max
    , minLength :: Maybe Min
    {-, pattern   :: Maybe Regex-}
    } deriving (Eq, Show, Read, Generic)

instance Monoid StringSchema where
    mempty = StringSchema mempty mempty
    a `mappend` b = StringSchema { maxLength = maxLength a <> maxLength b
                                 , minLength = minLength a <> minLength b
                                 }

instance ToJSON StringSchema where
    toJSON ss = object $ catMaybes [ ("maxLength" .=) <$> (toJSON . unMax <$> maxLength ss)
                                   , ("minLength" .=) <$> (toJSON . unMin <$> minLength ss)
                                   ]

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
