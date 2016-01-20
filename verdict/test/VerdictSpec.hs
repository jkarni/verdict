{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module VerdictSpec (spec) where

import Control.Exception
import GHC.Generics
import Data.Either
import Test.Hspec
import Verdict

spec :: Spec
spec = describe "Verdict" $ do
    maximumSpec
    minimumSpec
    maxLengthSpec
    minLengthSpec
    lengthSpec
    multipleOfSpec
    genericSpec

maximumSpec :: Spec
maximumSpec = describe "Maximum" $ do

  it "rejects larger values" $ do
    (validate 5 :: Either ErrorTree (Validated (Maximum 3) Integer))
        `shouldSatisfy` isLeft

  it "accepts smaller values" $ do
    (validate 3 :: Either ErrorTree (Validated (Maximum 5) Integer))
        `shouldSatisfy` isRight

  it "accepts equal sized values" $ do
    (validate 3 :: Either ErrorTree (Validated (Maximum 3) Integer))
        `shouldSatisfy` isRight

minimumSpec :: Spec
minimumSpec = describe "Minimum" $ do

  it "rejects smaller values" $ do
    (validate 3 :: Either ErrorTree (Validated (Minimum 5) Integer))
        `shouldSatisfy` isLeft

  it "accepts larger values" $ do
    (validate 5 :: Either ErrorTree (Validated (Minimum 3) Integer))
        `shouldSatisfy` isRight

  it "accepts equal sized values" $ do
    (validate 3 :: Either ErrorTree (Validated (Minimum 3) Integer))
        `shouldSatisfy` isRight

maxLengthSpec :: Spec
maxLengthSpec = describe "MaxLength" $ do

  it "rejects longer values" $ do
    (validate [(),()] :: Either ErrorTree (Validated (MaxLength 1) [()]))
        `shouldSatisfy` isLeft

  it "accepts shorter values" $ do
    (validate [()] :: Either ErrorTree (Validated (MaxLength 3) [()]))
        `shouldSatisfy` isRight

  it "accepts equal sized values" $ do
    (validate [()] :: Either ErrorTree (Validated (MaxLength 1) [()]))
        `shouldSatisfy` isRight

minLengthSpec :: Spec
minLengthSpec = describe "MinLength" $ do

  it "rejects shorter values" $ do
    (validate [()] :: Either ErrorTree (Validated (MinLength 2) [()]))
        `shouldSatisfy` isLeft

  it "accepts shorter values" $ do
    (validate [(), ()] :: Either ErrorTree (Validated (MinLength 1) [()]))
        `shouldSatisfy` isRight

  it "accepts equal sized values" $ do
    (validate [()] :: Either ErrorTree (Validated (MinLength 1) [()]))
        `shouldSatisfy` isRight

lengthSpec :: Spec
lengthSpec = describe "Length" $ do

  it "rejects differing lengths" $ do
    (validate [()] :: Either ErrorTree (Validated (Length 2) [()]))
        `shouldSatisfy` isLeft

  it "accepts matching lengths" $ do
    (validate [()] :: Either ErrorTree (Validated (Length 1) [()]))
        `shouldSatisfy` isRight

multipleOfSpec :: Spec
multipleOfSpec = describe "MultipleOf" $ do

  it "rejects non-multiples" $ do
    (validate 5 :: Either ErrorTree (Validated (MultipleOf 2) Integer))
        `shouldSatisfy` isLeft

  it "accepts multiples" $ do
    (validate 4 :: Either ErrorTree (Validated (MultipleOf 2) Integer))
        `shouldSatisfy` isRight

genericSpec :: Spec
genericSpec = describe "Generic instance" $ do

  context "Product types" $ do

    it "validates each field" $ do
      verdict badPerson `shouldBe`
        Just ((Leaf "Should be of length 10") `And` (Leaf "Should be more than 10"))

    it "accepts valid values" $ do
      verdict goodPerson `shouldBe` Nothing

  context "Sum types" $ do

    it "validates each field" $ do
      verdict badE1 `shouldBe` Just (Leaf "Should be of length 10")
      verdict badE2 `shouldBe` Just (Leaf "Should be more than 10")

    it "accepts valid values" $ do
      verdict goodE `shouldBe` Nothing

------------------------------------------------------------------------------

type String10 = Validated (Length 10) String
type IntegerMin10 = Validated (Minimum 10) Integer

data Person = Person { name :: String10
                     , age  :: IntegerMin10
                     } deriving (Generic, Eq, Show, Read, Verdict)

badPerson :: Person
badPerson = Person { name = unsafeValidated "a" , age = unsafeValidated 3 }

goodPerson :: Person
goodPerson = Person { name = unsafeValidated (replicate 10 'a') , age = unsafeValidated 20 }

data E = E1 String10
       | E2 IntegerMin10
  deriving (Generic, Eq, Show, Read, Verdict)

badE1 :: E
badE1 = E1 $ unsafeValidated "a"

badE2 :: E
badE2 = E2 $ unsafeValidated 4

goodE :: E
goodE = E1 . unsafeValidated $ replicate 10 'a'
