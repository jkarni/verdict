{-# LANGUAGE DeriveFunctor #-}
module Verdict.Types where

import Data.Monoid
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

data Failure e a = Failure e | Success a
  deriving (Eq, Show, Functor)

class Applicative m => ApplicativeError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance Monoid e => Applicative (Failure e) where
    pure                            = Success
    Failure msgs  <*> Failure msgs' = Failure (msgs <> msgs')
    Success _     <*> Failure msgs' = Failure msgs'
    Failure msgs' <*> Success _     = Failure msgs'
    Success f     <*> Success x     = Success (f x)

instance Monoid e => ApplicativeError e (Failure e) where
    throwError               = Failure
    catchError (Failure e) f = f e
    catchError s           _ = s

instance ApplicativeError e (Either e) where
    throwError            = Left
    catchError (Left e) f = f e
    catchError s        _ = s
