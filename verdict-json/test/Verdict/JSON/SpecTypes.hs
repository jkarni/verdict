{-# LANGUAGE OverloadedStrings #-}
module Verdict.JSON.SpecTypes where

import Data.Aeson
import Data.Proxy
import qualified Data.Map as Map
import           GHC.Generics (Generic)
import           Data.Vector  (fromList)

import Verdict
import Verdict.JSON

type Name  = Validated (MinLength 1 :&& MaxLength 100) String
type Age   = Validated (Minimum 0 :&& Maximum 200) Integer

data Person = Person
    { name :: Name
    , age  :: Age
    } deriving (Eq, Show, Generic, ToJSON)

instance JsonSchema Person where
    type JsonType Person = ObjectSchema
    jsonSchema' _ = mempty { properties = Map.fromList
                                [ ("name", (Required, jsonSchema namep))
                                , ("age" , (Required, jsonSchema agep ))
                                ]
                          }
      where namep = Proxy :: Proxy Name
            agep  = Proxy :: Proxy Age

type EvenInt = Validated (MultipleOf 2) Int
