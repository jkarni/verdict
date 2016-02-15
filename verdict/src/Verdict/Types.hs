{-# LANGUAGE DeriveFunctor #-}
module Verdict.Types where

import Data.Algebra.Boolean.FreeBoolean
import qualified Data.Text as Text
import Data.Reflection hiding (Z)
import Data.Proxy

------------------------------------------------------------------------------
-- * Logical Base Terms
------------------------------------------------------------------------------
data a :&& b = a :&& b
infixr 3 :&&
data a :|| b = a :|| b
infixr 2 :||
data Not a

------------------------------------------------------------------------------
-- * Datatype Representation Terms
------------------------------------------------------------------------------

data a :* b
infixr 5 :*

------------------------------------------------------------------------------
-- * Other Base Terms
------------------------------------------------------------------------------

data Minimum a
data Maximum a
data MaxLength a
data MinLength a
data Length a
data MultipleOf a
data Equals a

data HasElem a

------------------------------------------------------------------------------
-- * Other Types
------------------------------------------------------------------------------
data S n
data Z

instance Reifies Z Int where
  reflect _ = 0

instance (Reifies n Int) => Reifies (S n) Int where
  reflect _ = (+ 1) $ reflect (Proxy :: Proxy n)

type ErrorTree = FreeBoolean Text.Text

-- | Report a simple (non-decomposable) error
simpleError :: Text.Text -> ErrorTree
simpleError = FBValue

-- | No error, and nothing to report in negation.
noError :: ErrorTree
noError = FBTrue

-- | Error without any information. Prefer 'simpleError'.
absoluteError :: ErrorTree
absoluteError = FBFalse

isError :: ErrorTree -> Bool
isError = not . toBoolWith (const False)
