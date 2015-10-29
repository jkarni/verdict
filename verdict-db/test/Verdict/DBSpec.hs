{-# LANGUAGE DataKinds #-}
module Verdict.DBSpec (spec) where

import Verdict.DB
import Test.Hspec

spec :: Spec
spec = describe "Verdict.DB" $ do
    querySpec

querySpec :: Spec
querySpec = describe "query" $ do

  let ptable = insert [1..100] (empty :: VerdictDB '[Length] Int)
  it "returns only data that matches" $ do

    let r :: [Person LongName Adult]
        r = query ptable

  it "return all data that matches"

type LongName = Validated (MinLength 30) String
type Adult = Validated (Minimum 18) Int

data Person a b = Person { name :: a , age :: b }
    deriving (Eq, Show, Read)
