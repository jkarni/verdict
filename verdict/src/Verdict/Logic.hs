{-# LANGUAGE UndecidableInstances #-}
module Verdict.Logic where

import GHC.Exts (Constraint)
import Verdict.Types

type family Implies a b :: Constraint

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    Or () x = ()
    Or x () = ()

type instance Implies (a :&& b) c = (a `Implies` c) `Or` (b `Implies` c)
