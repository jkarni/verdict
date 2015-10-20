{-# LANGUAGE OverloadedStrings #-}
module Verdict.JSON.Types
    ( JsonConstraint(..)
    , JSONKey
    , ValidJSONKey
    , JsonSpec(..)
    ) where

import           Data.Aeson
import qualified Data.Map  as Map
import qualified Data.Text as Text
import           Verdict

data JsonConstraint a
    = Minimum Int
    | Maximum Int
    | MaxLength Int
    | MinLength Int
    | OtherError a
    deriving (Eq, Show, Read, Functor)

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
