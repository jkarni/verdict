{-# LANGUAGE UndecidableInstances #-}
module VerdictSpec where

import Data.Either
import Data.Proxy
import GHC.Generics (Generic)
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
  predSpec

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

safeCoerceSpec :: Spec
safeCoerceSpec = describe "safeCoerce" $ do
  let a = unsafeCoerce [1..10] :: Validated (Length 10) [Int]

  context "Validated" $ do

    it "accepts valid coercions" $ do
      shouldTypeCheck (safeCoerce a :: Validated (MaxLength 20) [Int])

  context "simply-polymorphic datatypes" $ do

    it "accepts valid coercions" $ do
      shouldTypeCheck (safeCoerce (A a) :: A (Validated (MaxLength 20) [Int]))

  {-context "non-polymorphic datatypes" $ do-}

    {-it "accepts valid coercions" $ do-}
      {-shouldTypeCheck (safeCoerce (B a) :: B)-}

  {-context "connected polymorphic datatypes" $ do-}

    {-it "accepts valid coercions" $ do-}
      {-shouldTypeCheck (safeCoerce (C a) :: C (Validated (MaxLength 20) [Int]))-}

  {-context "recursive polymorphic datatypes" $ do-}

    {-it "accepts valid coercions" $ do-}
      {-shouldTypeCheck (safeCoerce (DCons a DNull) :: D (Validated (MaxLength 20) [Int]))-}


predSpec :: Spec
predSpec = describe "GPred" $ do

  let a = unsafeCoerce [1..10] :: Validated (Length 10) [Int]

  context "simply-polymorphic datatypes" $ do

    it "has the correct Pred" $ do
      A a `shouldHavePred` (Proxy :: Proxy (TProd '[Length 10]))

  {-context "non-polymorphic datatypes" $ do-}

    {-it "has the correct Pred" $ do-}
      {-B a `shouldHavePred` (Proxy :: Proxy (TProd '[Length 10]))-}

  {-context "connected polymorphic datatypes" $ do-}

    {-it "has the correct Prod" $ do-}
      {-C a a `shouldHavePred` (Proxy :: Proxy (TProd '[Length 10, Length 10] ))-}

------------------------------------------------------------------------------
shouldTypeCheck :: a -> Expectation
shouldTypeCheck _ = True `shouldBe` True

shouldHavePred :: (Pred a ~ c) => a -> Proxy c -> Expectation
shouldHavePred _ _ = True `shouldBe` True

data A a = A { unA :: a }
  deriving (Eq, Show, Generic)

data B = B { unB :: Validated (Length 10) [Int] }
  deriving (Eq, Show, Generic)

data C a = C { c1 :: a, c2 :: a }
  deriving (Eq, Show, Generic)

data D a = DNull | DCons a (D a)
  deriving (Eq, Show, Generic)

instance (Verdict a b) => Verdict (A a) (A b) where
    type Pred (A a) = DefaultPred (A a)

{-instance Verdict B B where-}
    {-type Pred B = DefaultPred B-}

instance (Verdict a b) => Verdict (C a) (C b) where
    type Pred (C a) = DefaultPred (C a)

{-instance (Verdict a b) => Verdict (D a) (D b) where-}
    {-type Pred (D a) = DefaultPred (D a)-}
