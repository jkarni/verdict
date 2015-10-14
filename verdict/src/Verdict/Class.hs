module Verdict.Class where

import Control.Applicative
import Data.Proxy
import Data.Monoid
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
    haskVerdict _ x = guard (odd x) >> pure (Leaf "Should be even")

instance (Eq i, Num i) => HaskVerdict IsNonZero i where
    haskVerdict _ x = guard (x == 0) >> pure (Leaf "Should be non-zero")
