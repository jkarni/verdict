{-# LANGUAGE OverloadedStrings #-}
module Verdict.JSONSpec (spec) where

import Verdict
import Verdict.JSON ()
import Test.Hspec (Spec, it, describe, shouldBe, shouldContain)
import Data.Aeson (decode, eitherDecode)

type EvenInt = Validated (MultipleOf 2) Int

spec :: Spec
spec = describe "Verdict.JSON" $ do

    it "does validation when parsing" $ do
        (decode "5" :: Maybe EvenInt) `shouldBe` Nothing

    it "gives a useful error message" $ do
        let Left e = eitherDecode "5" :: Either String EvenInt
        e `shouldContain` "Not a multiple of 2"

    it "parses valid values" $ do
        let (Right exp) = val 4
        (decode "4" :: Maybe EvenInt) `shouldBe` Just exp
