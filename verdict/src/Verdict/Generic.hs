module Verdict.Generic where

import GHC.Generics


{-
type family (++) a b where
    as ++ (b ': bs) = (as ': b) ++ bs
    as ++ [] = as
-}
