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
    readSpec

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
      let p1 = Person { name = unsafeValidated (replicate 20 'a')
                      , age  = unsafeValidated 5
                      }
          p2 = Person { name = unsafeValidated (replicate 5 'a')
                      , age  = unsafeValidated 20
                      }
      verdict p1 `shouldBe` Just ((Leaf "Should be of length 10")
                            `And` (Leaf "Should be more than 10"))
      verdict p2 `shouldBe` Just (Leaf "Should be of length 10")

  context "Sum types" $ do

    it "validates each field" $ do
      let e1  = E1 . unsafeValidated $ replicate 20 'a'
          e2  = E2 $ unsafeValidated 5
          e1' = E1 . unsafeValidated $ replicate 10 'a'
          e2' = E2 $ unsafeValidated 20
      verdict e1 `shouldBe` Just (Leaf "Should be of length 10")
      verdict e2 `shouldBe` Just (Leaf "Should be more than 10")
      verdict e1' `shouldBe` Nothing
      verdict e2' `shouldBe` Nothing

{-
readSpec :: Spec
readSpec = describe "Read instance" $ do

  context "Product types" $ do

    it "validates each field" $ do
      evaluate (read "Person {name = \"aaaaaaaaaaaaaaaaaaaa\", age = 5}" :: Person)
        {-`shouldReturn` Person (unsafeValidated "aaaaaaaaaa") (unsafeValidated 20)-}
        `shouldThrow` errorCall "Leaf \"Should be of length 10\""
      {-evaluate (read "Person {name = \"aaaaaaaaaa\", age = 20}" :: Person)-}
        {-`shouldReturn` Person (unsafeValidated "aaaaaaaaaa") (unsafeValidated 20)-}
-}
------------------------------------------------------------------------------

type String10 = Validated (Length 10) String
type IntegerMin10 = Validated (Minimum 10) Integer

data Person = Person { name :: String10
                     , age  :: IntegerMin10
                     } deriving (Generic, Eq, Show, Read, Verdict)

data E = E1 String10
       | E2 IntegerMin10
  deriving (Generic, Eq, Show, Read, Verdict)
