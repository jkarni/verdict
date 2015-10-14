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
data IsNonZero -- etc

newtype Fix f a = Fix { unFix :: f (Fix f a) }

data ErrorTree e
  = Leaf e
  | Or (ErrorTree e) (ErrorTree e)
  | And (ErrorTree e) (ErrorTree e)
