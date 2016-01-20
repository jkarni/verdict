{-# LANGUAGE OverloadedStrings #-}
module Verdict.Class where

import           Control.Monad
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text     as Text
import           GHC.Generics
import           GHC.TypeLits
import           Verdict.Types

class Verdict v where
    verdict :: v -> Maybe ErrorTree
    default verdict :: (GVerdict (Rep v), Generic v) => v -> Maybe ErrorTree
    verdict = defaultVerdict


instance Verdict Int where
    verdict = const Nothing
instance Verdict Double where
    verdict = const Nothing
instance Verdict Float where
    verdict = const Nothing
instance Verdict Char where
    verdict = const Nothing
instance Verdict Bool where
    verdict = const Nothing
instance Verdict () where
    verdict = const Nothing

------------------------------------------------------------------------------
-- * HaskVerdict {{{
------------------------------------------------------------------------------
class HaskVerdict a b where
    haskVerdict :: Proxy a -> b -> Maybe ErrorTree

------------------------------------------------------------------------------
-- ** Logical Base Terms {{{
------------------------------------------------------------------------------
instance (HaskVerdict a r, HaskVerdict b r) => HaskVerdict (a :&& b) r where
    haskVerdict _ x =
      case (haskVerdict pa x, haskVerdict pb x) of
        (Just ex, Just ey) -> Just (And ex ey)
        (Just ex, _) -> Just ex
        (_, Just ey) -> Just ey
        _ -> Nothing
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

-- }}}
------------------------------------------------------------------------------
-- ** Other Base Terms {{{
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

-- }}}
-- }}}
------------------------------------------------------------------------------
-- * GVerdict {{{

class GVerdict (v :: * -> *) where
    gverdict :: v x -> Maybe ErrorTree

instance (GVerdict a, GVerdict b) => GVerdict (a :*: b) where
    gverdict (a :*: b) = case gverdict a of   --TODO: keep loc
      Nothing -> gverdict b
      Just ea -> case gverdict b of
        Nothing -> Just ea
        Just eb -> Just (ea `And` eb)

instance (GVerdict a, GVerdict b) => GVerdict (a :+: b) where
    gverdict (L1 a) = gverdict a -- TODO: keep loc
    gverdict (R1 b) = gverdict b -- TODO: keep loc

instance GVerdict a => GVerdict (M1 i c a) where
    gverdict (M1 a) = gverdict a -- TODO: keep loc

instance Verdict a => GVerdict (K1 i a) where
    gverdict (K1 a) = verdict a -- TODO: keep loc

defaultVerdict :: (Generic v, GVerdict (Rep v)) => v -> Maybe ErrorTree
defaultVerdict = gverdict . from
-- }}}
------------------------------------------------------------------------------
-- Known Val
class KnownVal a b | a -> b where
    knownVal :: Proxy a -> b

instance KnownNat n => KnownVal n Integer where
    knownVal = natVal
