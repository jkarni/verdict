{-# LANGUAGE OverloadedStrings #-}
module Verdict.Class where

import           Data.Monoid
import           Data.Proxy
import qualified Data.Text     as Text
import qualified Data.Algebra.Boolean as B
import           GHC.TypeLits
import           Verdict.Types

------------------------------------------------------------------------------
-- * HaskVerdict {{{
------------------------------------------------------------------------------
class HaskVerdict a b where
    haskVerdict :: Proxy a -> b -> ErrorTree

------------------------------------------------------------------------------
-- ** Logical Base Terms {{{
------------------------------------------------------------------------------
instance (HaskVerdict a r, HaskVerdict b r) => HaskVerdict (a :&& b) r where
    haskVerdict _ x = haskVerdict pa x B.&& haskVerdict pb x
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

instance (HaskVerdict a r, HaskVerdict b r) => HaskVerdict (a :|| b) r where
    haskVerdict _ x = haskVerdict pa x B.|| haskVerdict pb x
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

instance (HaskVerdict c a) => HaskVerdict (Not c) a where
    haskVerdict _ x = B.not $ haskVerdict p x
      where p = Proxy :: Proxy c

instance HaskVerdict 'True a where
    haskVerdict _ _ = noError

instance HaskVerdict 'False a where
    haskVerdict _ _ = noError

-- }}}
------------------------------------------------------------------------------
-- ** Other Base Terms {{{
------------------------------------------------------------------------------
instance HaskVerdict () a where
    haskVerdict _ _ = noError

instance (Ord b, Show b, KnownVal a b) => HaskVerdict (Maximum a) b where
    haskVerdict _ = check (<= p) ("Should be less than " <> showT p)
      where p = knownVal (Proxy :: Proxy a)

instance (Ord b, Show b, KnownVal a b) => HaskVerdict (Minimum a) b where
    haskVerdict _ = check (>= p) ("Should be more than " <> showT p)
      where p = knownVal (Proxy :: Proxy a)

instance (Foldable f, Show (f b), KnownNat a)
       => HaskVerdict (MaxLength a) (f b) where
    haskVerdict _ = check ((<= p) . length)
                          ("Should be of length less than " <> showT p)
      where p = fromInteger $ natVal (Proxy :: Proxy a)

instance (Foldable f, Show (f b), KnownNat a)
       => HaskVerdict (MinLength a) (f b) where
    haskVerdict _ = check ((>= p) . length)
                          ("Should be of length more than " <> showT p)
      where p = fromInteger $ knownVal (Proxy :: Proxy a)

instance (Foldable t, KnownNat a) => HaskVerdict (Length a) (t b) where
    haskVerdict _ = check ((== p) . length) ("Should be of length " <> showT p)
      where p = fromInteger $ natVal (Proxy :: Proxy a)

instance (Foldable t, Show b, Eq b, KnownVal a b)
    => HaskVerdict (HasElem a) (t b) where
    haskVerdict _ = check (elem p) ("Should contain " <> showT p)
      where p = knownVal (Proxy :: Proxy a)

instance (KnownNat n, Integral a) => HaskVerdict (MultipleOf n) a where
    haskVerdict _ = check (\x -> (toInteger x `rem` p) == 0) ("Not a multiple of " <> showT p)
      where p = natVal (Proxy :: Proxy n)

showT :: Show a => a -> Text.Text
showT = Text.pack . show

check :: (x -> Bool) -> Text.Text -> x -> ErrorTree
check pred' err x = let e = simpleError err
                    in if pred' x then B.not e else e

-- }}}
-- }}}
------------------------------------------------------------------------------
-- Known Val
class KnownVal a b | a -> b where
    knownVal :: Proxy a -> b

instance KnownNat n => KnownVal n Integer where
    knownVal = natVal
