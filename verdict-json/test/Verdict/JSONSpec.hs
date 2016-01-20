{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Verdict.JSONSpec (spec) where

import           Control.Lens
import           Data.Aeson
import           Data.Proxy
import           Data.Swagger
import           GHC.Generics (Generic)
import           Test.Hspec   (Spec, context, describe, it, shouldBe,
                               shouldContain)
import           Verdict

import           Verdict.JSON ()

spec :: Spec
spec = describe "Verdict.JSON" $ do
    fromJSONSpec
    genericSpec

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

    it "gives a useful error message" $ do
      let Left e = eitherDecode (encode badPerson) :: Either String Person
      e `shouldContain` "Should be of length more than 1"

    it "parses valid values" $ do
      (decode (encode goodPerson) :: Maybe Person) `shouldBe` Just goodPerson


genericSpec :: Spec
genericSpec = describe "Generic ToSchema" $ do

  let jspec = toSchema (Proxy :: Proxy Person)

  it "has the required properties list" $ do
    (jspec ^. schemaRequired) `shouldBe` ["name", "age"]

  it "has the nested properties" $ do
    let Just (Inline i1) = jspec ^. schemaProperties . at "age"
    let Just (Inline i2) = jspec ^. schemaProperties . at "name"
    (i1 ^. schemaMaximum) `shouldBe` Just 200.0
    (i1 ^. schemaMinimum) `shouldBe` Just 0.0
    (i2 ^. schemaMinLength) `shouldBe` Just 1
    (i2 ^. schemaMaxLength) `shouldBe` Just 100

type EvenInt = Validated (MultipleOf 2) Int

type Name  = Validated (MinLength 1 :&& MaxLength 100) String
type Age   = Validated (Minimum 0 :&& Maximum 200) Integer

data Person = Person
    { name :: Name
    , age  :: Age
    } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

badPerson :: Person
badPerson = Person { name = unsafeValidated "", age = unsafeValidated 250 }

goodPerson :: Person
goodPerson = Person { name = unsafeValidated "Kilroy"
                    , age = unsafeValidated 20 }
