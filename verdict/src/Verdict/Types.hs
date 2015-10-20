{-# LANGUAGE DeriveFunctor #-}
module Verdict.Types where

import qualified Data.Text as Text

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

data HasElem a

------------------------------------------------------------------------------
-- * Other Types
------------------------------------------------------------------------------

-- | Error representation
data ErrorTree' e
  = Leaf e
  | Or  (ErrorTree' e) (ErrorTree' e)
  | And (ErrorTree' e) (ErrorTree' e)
  | Sum (ErrorTree' e) (ErrorTree' e)
  deriving (Eq, Show, Functor)

type ErrorTree = ErrorTree' Text.Text

instance Monoid e => Monoid (ErrorTree' e) where
    mempty  = Leaf mempty
    mappend = And
