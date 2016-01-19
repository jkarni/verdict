{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Verdict.JSONSpec (spec) where

import           Data.Aeson
import           Data.Foldable (toList)
import qualified Data.Map     as Map
import qualified Data.HashMap.Strict as HashMap
import           Data.Proxy
import           GHC.Generics (Generic)
import           Test.Hspec   (Spec, describe, it, shouldBe, shouldContain, context)
import           Verdict

import           Verdict.JSON

spec :: Spec
spec = describe "Verdict.JSON" $ do
    fromJSONSpec
    specSpec


fromJSONSpec :: Spec
fromJSONSpec = describe "FromJSON instance" $ do

  context "Validated" $ do

    it "does validation when parsing" $ do
      (decode "5" :: Maybe EvenInt) `shouldBe` Nothing
      (decode "4" :: Maybe EvenInt) `shouldBe` Just (unsafeValidated 4)

    it "gives a useful error message" $ do
      let Left e = eitherDecode "5" :: Either String EvenInt
      e `shouldContain` "Not a multiple of 2"

    it "parses valid values" $ do
      let (Right expected) = validate 4
      (decode "4" :: Maybe EvenInt) `shouldBe` Just expected

  context "Generic" $ do

    it "does validation when parsing" $ do
      (decode (encode badPerson) :: Maybe Person) `shouldBe` Nothing
      (decode (encode goodPerson) :: Maybe Person) `shouldBe` Just goodPerson

specSpec :: Spec
specSpec = describe "AnySchema" $ do

  context "ToJSON instance" $ do
    let (Object jspec)        = toJSON $ jsonSchema (Proxy :: Proxy Person)
        (Just (Object props)) = HashMap.lookup "properties" jspec
        (Just (Array reqs))   = HashMap.lookup "required" jspec
        (Just (Object ageO))  = HashMap.lookup "age" props
        (Just (Object nameO)) = HashMap.lookup "name" props

    it "lists required properties " $ do
      toList reqs `shouldContain` [String "name"]
      toList reqs `shouldContain` [String "age"]

    it "contains the outermost type" $ do
      HashMap.lookup "type" jspec `shouldBe` Just (String "object")

    it "contains the nested types" $ do
      HashMap.lookup "type" nameO `shouldBe` Just (String "string")
      HashMap.lookup "type" ageO  `shouldBe` Just (String "number")

    it "contains the nested constraints" $ do
      HashMap.lookup "minLength" nameO `shouldBe` Just (Number 1)
      HashMap.lookup "maxLength" nameO `shouldBe` Just (Number 100)
      HashMap.lookup "minimum" ageO `shouldBe` Just (Number 0)
      HashMap.lookup "maximum" ageO `shouldBe` Just (Number 200)



type EvenInt = Validated (MultipleOf 2) Int

type Name  = Validated (MinLength 1 :&& MaxLength 100) String
type Age   = Validated (Minimum 0 :&& Maximum 200) Integer

data Person = Person
    { name :: Name
    , age  :: Age
    } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

badPerson :: Person
badPerson = Person { name = unsafeValidated "", age = unsafeValidated 250 }

goodPerson :: Person
goodPerson = Person { name = unsafeValidated "Kilroy"
                    , age = unsafeValidated 20 }

instance JsonSchema Person where
    type JsonType Person = ObjectSchema
    jsonSchema' _ = mempty { properties = Map.fromList
                                [ ("name", (Required, jsonSchema namep))
                                , ("age" , (Required, jsonSchema agep ))
                                ]
                          }
      where namep = Proxy :: Proxy Name
            agep  = Proxy :: Proxy Age
