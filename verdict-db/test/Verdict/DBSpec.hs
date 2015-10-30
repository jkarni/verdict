{-# LANGUAGE DataKinds #-}
module Verdict.DBSpec (spec) where

import Verdict
import Verdict.DB
import Test.Hspec

spec :: Spec
spec = describe "Verdict.DB" $ do
    querySpec

querySpec :: Spec
querySpec = describe "query" $ do

  let eg :: DB '[Length 5, Length 10] [Int]
      eg = insert [1..10] $ insert [2..6] $ insert [1..5] empty

  it "returns all data that matches" $ do

    let r1 :: [Validated (Length 5) [Int]]
        r1 = query eg
        r2 :: [Validated (Length 10) [Int]]
        r2 = query eg
    r1 `shouldBe` [unsafeValidated [2..6], unsafeValidated [1..5]]
    r2 `shouldBe` [unsafeValidated [1..10]]

