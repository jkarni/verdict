{-# LANGUAGE OverloadedStrings #-}
module Verdict.Class where

import           Control.Monad
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text     as Text
import           GHC.TypeLits
import           Verdict.Types

------------------------------------------------------------------------------
-- * HaskVerdict
------------------------------------------------------------------------------
class HaskVerdict a b where
    haskVerdict :: Proxy a -> b -> Maybe ErrorTree


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
-- * Other Base Terms
------------------------------------------------------------------------------
instance HaskVerdict () a where
    haskVerdict _ = const Nothing

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

check :: (x -> Bool) -> err -> x -> Maybe (ErrorTree' err)
check pred' err x = guard (not $ pred' x) >> pure (Leaf err)

------------------------------------------------------------------------------
-- Known Val
class KnownVal a b | a -> b where
    knownVal :: Proxy a -> b

instance KnownNat n => KnownVal n Integer where
    knownVal = natVal
