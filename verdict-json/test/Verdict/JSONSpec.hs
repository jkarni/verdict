{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Verdict.JSONSpec (spec) where

import           Data.Aeson
import qualified Data.Map     as Map
import           Data.Proxy
import           Data.Vector  (fromList)
import           GHC.Generics (Generic)
import           Test.Hspec   (Spec, describe, it, shouldBe, shouldContain)
import           Verdict

import           Verdict.JSON

spec :: Spec
spec = describe "Verdict.JSON" $ do
    fromJSONSpec
    specSpec


fromJSONSpec :: Spec
fromJSONSpec = describe "FromJSON instance" $ do

  it "does validation when parsing" $ do
    (decode "5" :: Maybe EvenInt) `shouldBe` Nothing

  it "gives a useful error message" $ do
    let Left e = eitherDecode "5" :: Either String EvenInt
    e `shouldContain` "Not a multiple of 2"

  it "parses valid values" $ do
    let (Right expected) = val 4
    (decode "4" :: Maybe EvenInt) `shouldBe` Just expected

specSpec :: Spec
specSpec = describe "Spec" $ do

  it "has a ToJSON instance that is in JSON Schema format" $ do

    let props = object [ "name" .= object [ "type"      .= String "string"
                                          , "minLength" .= Number 1
                                          , "maxLength" .= Number 100
                                          ]
                       , "age"  .= object [ "type"    .= String "integer"
                                          , "minimum" .= Number 0
                                          , "maximum" .= Number 200
                                          ]
                       ]
    let expected = object [ "properties" .= props
                          , "required"   .= Array (fromList [ String "name"
                                                            , String "age"
                                                            ])
                          ]
    let jspec = jsonSchema (Proxy :: Proxy Person)
    print $ encode jspec
    toJSON jspec `shouldBe` expected


type EvenInt = Validated (MultipleOf 2) Int

type Name  = Validated (MinLength 1 :&& MaxLength 100) String
type Age   = Validated (Minimum 0 :&& Maximum 200) Integer

data Person = Person
    { name :: Name
    , age  :: Age
    } deriving (Eq, Show, Read, Generic, ToJSON)

instance JsonSchema Person where
    type JsonType Person = ObjectSchema
    jsonSchema' _ = mempty { properties = Map.fromList
                                [ ("name", (Required, jsonSchema namep))
                                , ("age" , (Required, jsonSchema agep ))
                                ]
                          }
      where namep = Proxy :: Proxy Name
            agep  = Proxy :: Proxy Age
