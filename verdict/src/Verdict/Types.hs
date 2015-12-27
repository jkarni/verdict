{-# LANGUAGE DeriveFunctor #-}
module Verdict.Types where

import qualified Data.Text as Text
import GHC.TypeLits

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
-- * Accessors
------------------------------------------------------------------------------
-- Apply constraints to each argument of a constructor. These are expected to
-- be in order, and have as many elements as the constructor has arguments
data TProd (a :: [k])
-- Apply constraints to each constructor
data TSum (a :: [k])
-- Apply constraints to record fields by name.
data TRec (a :: [*])
data Rec (recName :: Symbol) x

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
