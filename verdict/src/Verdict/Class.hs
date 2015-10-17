module Verdict.Class where

import Control.Applicative
import Data.Proxy
import Data.Monoid
import GHC.TypeLits
import Control.Monad
import qualified Generics.SOP as G
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

instance (HaskVerdict c a) => HaskVerdict (Not c) a where
    haskVerdict _ x = case haskVerdict p x of
        Just _ -> Nothing
        Nothing -> Just (Leaf "this still needs to be figured out")
      where p = Proxy :: Proxy c

instance HaskVerdict 'True a where
    haskVerdict _ _ = Nothing

------------------------------------------------------------------------------
-- * Datatype Representation Terms
------------------------------------------------------------------------------

{-instance ( G.Generic a ) => HaskVerdict (a :* b) r where-}
    {-haskVerdict _ x = foldM (G.hcollapse . G.hap r $ G.from x)-}
      {-where pa = Proxy :: Proxy a-}
            {-pb = Proxy :: Proxy b-}
            {-v1 = G.Fn $ \(G.I n) -> G.K (haskVerdict pa n)-}
            {-v2 = G.Fn $ \(G.I n) -> G.K (haskVerdict pb n)-}
            {-r  = (v1 G.:* v2 G.:* G.Nil) G.:* G.Nil-}

------------------------------------------------------------------------------
-- * Other Base Terms
------------------------------------------------------------------------------
instance HaskVerdict () a where
    haskVerdict _ = const Nothing

instance (Ord b, Show b, KnownVal a b) => HaskVerdict (Maximum a) b where
    haskVerdict _ = check (<= p) ("Should be less than " ++ show p)
      where p = knownVal (Proxy :: Proxy a)

instance (Ord b, Show b, KnownVal a b) => HaskVerdict (Minimum a) b where
    haskVerdict _ = check (>= p) ("Should be more than " ++ show p)
      where p = knownVal (Proxy :: Proxy a)

instance (Foldable t, KnownNat a) => HaskVerdict (Length a) (t b) where
    haskVerdict _ = check ((== p) . length) ("Should be of length " ++ show p)
      where p = fromInteger $ natVal (Proxy :: Proxy a)

instance (Foldable t, Show b, Eq b, KnownVal a b)
    => HaskVerdict (HasElem a) (t b) where
    haskVerdict _ = check (elem p) ("Should contain " ++ show p)
      where p = knownVal (Proxy :: Proxy a)

instance (KnownNat n, Integral a) => HaskVerdict (MultipleOf n) a where
    haskVerdict _ = check (\x -> (toInteger x `rem` p) == 0) ("Not a multiple of " ++ show p)
      where p = natVal (Proxy :: Proxy n)


check :: (x -> Bool) -> err -> x -> Maybe (ErrorTree err)
check pred err x = guard (not $ pred x) >> pure (Leaf err)

------------------------------------------------------------------------------
-- Known Val
class KnownVal a b | a -> b where
    knownVal :: Proxy a -> b

instance KnownNat n => KnownVal n Integer where
    knownVal = natVal
