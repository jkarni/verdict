{-# LANGUAGE ConstraintKinds #-}
module Verdict ( HaskVerdict(..)
               , val
               , getVal
               , Validated
               , Implies
               , Implies'
               , Or
               , (:&&)
               , (:||)
               , Not
               , Minimum
               , Maximum
               , MaxLength
               , MinLength
               , Length
               , MultipleOf
               , HasElem
               , ErrorTree(..)
               , unsafeCoerceVal
               , check
               , KnownVal(..)
               )  where

import Verdict.Class
import Verdict.Types
import Verdict.Logic
import Verdict.Val
