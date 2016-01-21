{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Verdict.Types where

import Data.Algebra.Boolean.FreeBoolean
import Data.Algebra.Boolean.NormalForm
import Data.Algebra.Boolean.DNF
import qualified Data.Text.Lazy as Text
import qualified Text.PrettyPrint.Leijen.Text as PP

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

prettyError' :: DNF Text.Text -> PP.Doc
prettyError' e = outer $ inner <$> toDoubleList e
  where
    inner = foldr (\new rest -> PP.text new PP.<> PP.text " AND " PP.<> rest) mempty
    outer = foldr (\new rest -> new PP.<> PP.text " OR " PP.<> rest) mempty

prettyError :: ErrorTree -> PP.Doc
prettyError e = prettyError' (fromFreeBoolean e :: DNF Text.Text)
