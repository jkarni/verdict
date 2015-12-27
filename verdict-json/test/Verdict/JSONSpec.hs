{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Verdict.JSONSpec (spec) where

import           Data.Aeson
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import           Data.Proxy
import           GHC.Generics (Generic)
import           Test.Hspec   (Spec, describe, it, shouldBe, shouldContain, context)
import           Verdict

import           Verdict.JSON
import           Verdict.JSON.SpecTypes

spec :: Spec
spec = describe "Verdict.JSON" $ do
    fromJSONSpec
    specSpec


fromJSONSpec :: Spec
fromJSONSpec = describe "FromJSON instance" $ do

  context "Validated" $ do

    it "does validation when parsing" $ do
      (decode "5" :: Maybe EvenInt) `shouldBe` Nothing

    it "gives a useful error message" $ do
      let Left e = eitherDecode "5" :: Either String EvenInt
      e `shouldContain` "Not a multiple of 2"

    it "parses valid values" $ do
      let (Right expected) = validate 4
      (decode "4" :: Maybe EvenInt) `shouldBe` Just expected

  context "other datatypes" $ do

    let x = A 5 (unsafeCoerce 2) :: A Int
        y = A 2 (unsafeCoerce 3) :: A Int
        z = A (unsafeCoerce 2) (unsafeCoerce 2) :: A EvenInt

    it "does validation when parsing" $ do
      (decode (encode x) :: Maybe (A EvenInt)) `shouldBe` Nothing

    it "gives a useful error message for polymorphic recors" $ do
      let Left e = eitherDecode (encode x) :: Either String (A EvenInt)
      e `shouldContain` "Not a multiple of 2"

    it "gives a useful error message for non-polymorphic recors" $ do
      let Left e = eitherDecode (encode y) :: Either String (A EvenInt)
      e `shouldContain` "Not a multiple of 2"

    it "parses valid values" $ do
      let Right e = eitherDecode (encode z) :: Either String (A EvenInt)
      e `shouldBe` z

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

genericSpec :: Spec
genericSpec = describe "default instance" $ do

  let (Object jspec) = toJSON $ jsonSchema (Proxy :: Proxy (B EvenInt))
      (Just (Object props)) = HashMap.lookup "properties" jspec
      (Just (Array reqs))   = HashMap.lookup "required" jspec

  it "lists all required records" $ do
    toList reqs `shouldContain` [String "unB"]

data A a = A { aa :: a, ab :: EvenInt }
  deriving (Eq, Show, Generic, Read, FromJSON, ToJSON)

data B a = B { unB :: a }
  deriving (Eq, Show, Generic, Read, FromJSON, ToJSON)

instance Verdict a b => Verdict (B a) (B b) where
    type Pred (B a) = DefaultPred (B a)
