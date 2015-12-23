{-# LANGUAGE UndecidableInstances #-}
module VerdictSpec (spec) where

import Data.Either
import Test.Hspec
import Verdict

spec :: Spec
spec = describe "Verdict" $ do
  context "validate" $ do
    maximumSpec
    minimumSpec
    maxLengthSpec
    minLengthSpec
    lengthSpec
    multipleOfSpec
  safeCoerceSpec

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
    (validate (5 :: Integer) :: Either ErrorTree (Validated (MultipleOf 2) Integer))
        `shouldSatisfy` isLeft

  it "accepts multiples" $ do
    (validate (4 :: Integer) :: Either ErrorTree (Validated (MultipleOf 2) Integer))
        `shouldSatisfy` isRight

safeCoerceSpec :: Spec
safeCoerceSpec = describe "safeCoerce" $ do
  let x = unsafeCoerce [1..10] :: Validated (Length 10) [Int]

  context "Validated" $ do

    it "accepts valid coercions" $ do
      shouldTypeCheck (safeCoerce x :: Validated (MaxLength 20) [Int])

  context "other datatypes" $ do

    it "accepts valid coercions" $ do
      shouldTypeCheck (safeCoerce (A x) :: A (Validated (MaxLength 20) [Int]))


------------------------------------------------------------------------------
shouldTypeCheck :: a -> Expectation
shouldTypeCheck _ = True `shouldBe` True

data A a = A { unA :: a }
  deriving (Eq, Show)

type instance VPred' (A a) (A b) = VPred a b

instance (Verdict a b) => Verdict (A a) (A b) where
  unsafeCoerce = A . unsafeCoerce . unA
  unvalidate   = A . unvalidate . unA
