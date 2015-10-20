{-# LANGUAGE OverloadedStrings #-}
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
    let (Right exp) = val 4
    (decode "4" :: Maybe EvenInt) `shouldBe` Just exp

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
    let spec = jsonSchema (Proxy :: Proxy Person)
    toJSON spec `shouldBe` expected


type EvenInt = Validated (MultipleOf 2) Int

type NameC = MinLength 1 :&& MaxLength 100
type Name  = Validated NameC String
type AgeC  = Minimum 0 :&& Maximum 200
type Age   = Validated AgeC Integer

data Person = Person
    { name :: Name
    , age  :: Age
    } deriving (Eq, Show, Read, Generic, ToJSON)

instance JsonSchema Person where
    jsonSchema _ = JsonSpec
        $ Map.fromList [ ("name", Left $ jsonVerdict namep)
                       , ("age" , Left $ jsonVerdict agep )
                       ]
      where namep = Proxy :: Proxy NameC
            agep  = Proxy :: Proxy AgeC
