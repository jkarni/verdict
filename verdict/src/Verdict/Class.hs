{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Verdict.Class where

import           Control.Monad
import           Data.Coerce (Coercible, coerce)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text     as Text
import           GHC.Generics
import           GHC.TypeLits
import           Verdict.Types

------------------------------------------------------------------------------
-- * Verdict
------------------------------------------------------------------------------

-- | Note: generally @unsafeCoerce@ and @unvalidate@ should be @coerce@, so
-- that there is no runtime cost to these operations.
class Verdict a base | a -> base where
    type Pred a
    -- Function to coerce without checking constraints. The runtime
    -- representation of @VerdictBase a@ and @a@ should be the same.
    -- This function only gets called by @Verdict@ when it is safe to do so.
    unsafeCoerce :: base -> a
    default unsafeCoerce :: (Generic a, Generic base, GVerdict (Rep a) (Rep base))
                         => base -> a
    unsafeCoerce = defaultUnsafeCoerce
    -- Function to throw away constraints.
    unvalidate   :: a -> base
    default unvalidate :: (Generic a, Generic base, GVerdict (Rep a) (Rep base))
                        => a -> base
    unvalidate = defaultUnvalidate


instance Verdict Int Int where
    type Pred Int = ()
instance Verdict Double Double where
    type Pred Double = ()
instance Verdict Float Float where
    type Pred Float = ()
instance Verdict Char Char where
    type Pred Char = ()
instance Verdict Bool Bool where
    type Pred Bool = ()
instance Verdict () () where
    type Pred () = ()


------------------------------------------------------------------------------
-- * HaskVerdict
------------------------------------------------------------------------------
class HaskVerdict a b where
    haskVerdict :: Proxy a -> b -> Maybe ErrorTree


------------------------------------------------------------------------------
-- * Logical Base Terms
------------------------------------------------------------------------------
instance (HaskVerdict a r, HaskVerdict b r) => HaskVerdict (a :&& b) r where
    haskVerdict _ x = case (haskVerdict pa x, haskVerdict pb x) of
        (Nothing, Nothing) -> Nothing
        (Just e1, Just e2) -> Just (e1 `And` e2)
        (Just e1, Nothing) -> Just e1
        (Nothing, Just e2) -> Just e2
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

------------------------------------------------------------------------------
-- * Accessors
------------------------------------------------------------------------------
-- instance HaskVerdict (x ': xs) v
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

------------------------------------------------------------------------------
-- Generics
------------------------------------------------------------------------------
class GVerdict (a :: * -> *) (b :: * -> *) | a -> b where
    type GPred a
    gunsafeCoerce :: b x -> a x
    gunvalidate   :: a x -> b x

instance GVerdict U1 U1 where
    type GPred U1 = ()
    gunsafeCoerce = id
    gunvalidate   = id

instance (GVerdict a a', GVerdict b b') => GVerdict (a :*: b) (a' :*: b') where
    type GPred (a :*: b) = TProd (a ': UnProd (GPred b))
    gunsafeCoerce (a :*: b) = gunsafeCoerce a :*: gunsafeCoerce b
    gunvalidate   (a :*: b) = gunvalidate a   :*: gunvalidate b

instance (GVerdict a a', GVerdict b b') => GVerdict (a :+: b) (a' :+: b') where
    type GPred (a :+: b) = TSum (a ': UnSum (GPred b))
    gunsafeCoerce (L1 a) = L1 $ gunsafeCoerce a
    gunsafeCoerce (R1 a) = R1 $ gunsafeCoerce a
    gunvalidate   (L1 a) = L1 $ gunvalidate a
    gunvalidate   (R1 a) = R1 $ gunvalidate a

instance (GVerdict a a') => GVerdict (M1 i c a) (M1 i c a') where
    type GPred (M1 i c a) = GPred a
    gunsafeCoerce (M1 a) = M1 $ gunsafeCoerce a
    gunvalidate   (M1 a) = M1 $ gunvalidate a

instance (Verdict a a') => GVerdict (K1 i a) (K1 i a') where
    type GPred (K1 i a) = TProd '[Pred a]
    gunsafeCoerce (K1 a) = K1 $ unsafeCoerce a
    gunvalidate   (K1 a) = K1 $ unvalidate a

type family UnProd a where
    UnProd (TProd a) = a

type family UnSum a where
    UnSum (TSum a) = a

defaultUnsafeCoerce :: (Generic a, Generic x, GVerdict (Rep a) (Rep x)) => x -> a
defaultUnsafeCoerce = to . gunsafeCoerce . from

defaultUnvalidate :: (Generic a, Generic x, GVerdict (Rep a) (Rep x)) => a -> x
defaultUnvalidate = to . gunvalidate . from

type DefaultPred a = GPred (Rep a)
