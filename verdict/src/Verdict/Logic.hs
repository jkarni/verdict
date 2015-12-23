{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Verdict.Logic where

import           GHC.Exts      (Constraint)
import           GHC.TypeLits
import           Verdict.Types


type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    Or () x = ()
    Or x () = ()

type family And (a :: Constraint) (b :: Constraint) :: Constraint where
    And () b = ()
    And b () = ()

type family Implies (a :: k1) (b :: k2) :: Constraint where
    Implies a a = ()
    Implies a () = ()
    Implies () a = a
    Implies (a :&& b) c = (a `Implies` c) `Or`  (b `Implies` c)
    Implies (a :|| b) c = (a `Implies` c) `And` (b `Implies` c)
    Implies a (b :|| c) = (a `Implies` b) `Or`  (a `Implies` c)
    Implies a (b :&& c) = (a `Implies` b) `And` (a `Implies` c)
    Implies a (Not a) = ('True ~ 'False)
    Implies (Not a) a = ('True ~ 'False)
    Implies a (Not (Not a)) = ()
    Implies a 'True = ()
    Implies a b = Implies' a b

type family Implies' a b :: Constraint
type instance Implies' (Length a) (MaxLength b) = a <= b
type instance Implies' (Length a) (MinLength b) = b <= b
type instance Implies' (MaxLength a) (MaxLength b) = a <= b
type instance Implies' (MinLength a) (MinLength b) = b <= a
