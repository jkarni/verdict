module Verdict.Class where

import Data.Proxy
import Data.Monoid
import Control.Monad
import Verdict.Types

------------------------------------------------------------------------------
-- * HaskVerdict
------------------------------------------------------------------------------
class HaskVerdict a b where
    haskVerdict :: Proxy a -> b -> Maybe Errors

------------------------------------------------------------------------------
-- * Logical Base Terms
------------------------------------------------------------------------------
instance (HaskVerdict a r, HaskVerdict b r) => HaskVerdict (a :&& b) r where
    haskVerdict _ x = haskVerdict pa x <> haskVerdict pb x
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

-- The introduction rules for :|| requires some delving into okmij.org to
-- implement

------------------------------------------------------------------------------
-- * Other Base Terms
------------------------------------------------------------------------------
instance HaskVerdict () a where
    haskVerdict _ = const Nothing

instance Integral i => HaskVerdict IsEven i where
    haskVerdict _ x = guard (odd x) >> pure ["Should be even"]

instance (Eq i, Num i) => HaskVerdict IsNonZero i where
    haskVerdict _ x = guard (x == 0) >> pure ["Should be non-zero"]
