module Verdict.Class where

import Control.Applicative
import Data.Proxy
import Data.Monoid
import GHC.TypeLits
import Control.Monad
import Verdict.Types

------------------------------------------------------------------------------
-- * HaskVerdict
------------------------------------------------------------------------------
class HaskVerdict a b where
    haskVerdict :: Proxy a -> b -> Maybe (ErrorTree String)

------------------------------------------------------------------------------
-- * Logical Base Terms
------------------------------------------------------------------------------
instance (HaskVerdict a r, HaskVerdict b r) => HaskVerdict (a :&& b) r where
    haskVerdict _ x = And <$> haskVerdict pa x <*> haskVerdict pb x
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

instance (HaskVerdict a r, HaskVerdict b r) => HaskVerdict (a :|| b) r where
    haskVerdict _ x = Or <$> haskVerdict pa x <*> haskVerdict pb x
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b


------------------------------------------------------------------------------
-- * Other Base Terms
------------------------------------------------------------------------------
instance HaskVerdict () a where
    haskVerdict _ = const Nothing

instance Integral i => HaskVerdict IsEven i where
    haskVerdict _ = check even "Should be even"

instance (Eq i, Num i) => HaskVerdict IsNonZero i where
    haskVerdict _ = check (/= 0) "Should be non-zero"

instance (Ord b, Show b, KnownVal a b) => HaskVerdict (Maximum a) b where
    haskVerdict _ = check (<= p) ("Should be less than " ++ show p)
      where p = knownVal (Proxy :: Proxy a)

instance (Ord b, Show b, KnownVal a b) => HaskVerdict (Minimum a) b where
    haskVerdict _ = check (>= p) ("Should be more than " ++ show p)
      where p = knownVal (Proxy :: Proxy a)

instance (Foldable t, KnownNat a) => HaskVerdict (Length a) (t b) where
    haskVerdict _ = check ((== p) . length) ("Should be of length " ++ show p)
      where p = fromInteger $ natVal (Proxy :: Proxy a)

check :: (x -> Bool) -> err -> x -> Maybe (ErrorTree err)
check pred err x = guard (not $ pred x) >> pure (Leaf err)

------------------------------------------------------------------------------
-- Known Val
class KnownVal a b | a -> b where
    knownVal :: Proxy a -> b
