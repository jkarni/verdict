{-# LANGUAGE ConstraintKinds #-}
module Verdict ( HaskVerdict(..)
               , val
               , getVal
               , Validated
               , Implies
               , Or
               , (:&&)
               , (:||)
               , Not
               , IsEven
               , IsNonZero
               , Minimum
               , Maximum
               , MaxLength
               , MinLength
               , Length
               , ErrorTree(..)
               , unsafeCoerceVal
               , check
               , KnownVal(..)
               )  where

import Verdict.Class
import Verdict.Types
import Verdict.Logic
import Verdict.Val


eg :: (x `Implies` IsEven, Integral a) => a -> Validated x a -> a
eg a (Validated b) = a `div` b
