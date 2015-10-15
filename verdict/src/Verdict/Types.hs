{-# LANGUAGE DeriveFunctor #-}
module Verdict.Types where


------------------------------------------------------------------------------
-- * Logical Base Terms
------------------------------------------------------------------------------
data a :&& b = a :&& b
infixr 3 :&&
data a :|| b = a :|| b
infixr 2 :||
data Not a

------------------------------------------------------------------------------
-- * Other Base Terms
------------------------------------------------------------------------------

data IsEven
data IsNonZero
data Minimum a
data Maximum a
data MaxLength a
data MinLength a
data Length a

data HasElem a

data ErrorTree e
  = Leaf e
  | Or (ErrorTree e) (ErrorTree e)
  | And (ErrorTree e) (ErrorTree e)
  deriving (Eq, Show, Functor)
