{-# LANGUAGE UndecidableInstances #-}
module Verdict.Base.Internal.Monoid where

import qualified Prelude as P
import GHC.TypeLits
import Data.Proxy

import Verdict.Types
import Verdict.Logic
import Verdict.Val


class Monoid (m :: k) where
    type Mappend m (a :: k) (b :: k) :: k
    type Mempty  m :: k

instance Monoid (m :: Nat) where
    type Mempty m = 0
    type Mappend m a b = a + b

class MonoidHomomorphism (m :: k1 -> k2) (b :: P.Bool)
instance MonoidHomomorphism m 'P.False
instance MonoidHomomorphism Length 'P.True


mappend :: (Monoid m, P.Monoid a)
        => Proxy m -> Validated c1 a -> Validated c2 a -> Validated (Mappend m c1 c2) a
mappend _ (Validated a) (Validated b) = unsafeValidated (a `P.mappend` b)

{-
mempty :: (P.Monoid a) => Validated mempty a
mempty = unsafeValidated P.mempty
-}

-- E.g.
t1 :: Validated (Length 5) [P.Integer]
t1 = unsafeValidated [1..5]

t2 :: Validated (Length 7) [P.Integer]
t2 = unsafeValidated [1..7]


t3 :: Validated (Length 12) [P.Integer]
t3 = mappend (Proxy :: Proxy (Proxy Nat)) t1 t2
