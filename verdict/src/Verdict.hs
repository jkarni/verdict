module Verdict ( HaskVerdict(..)
               , val
               , getVal
               , Validated
               , Implies
               , Or
               , (:&&)
               , (:||)
               , IsEven
               , Errors
               , IsNonZero
               )  where

import Verdict.Class
import Verdict.Types
import Verdict.Logic
import Verdict.Val


eg :: (x `Implies` IsEven, Integral a) => a -> Validated x a -> a
eg a (Validated b) = a `div` b
