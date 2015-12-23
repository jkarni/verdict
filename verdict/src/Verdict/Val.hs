{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Verdict.Val where

import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Coerce       (Coercible, coerce)
import           Data.Either       (isRight)
import           Data.Foldable
import           Data.Proxy
import           Data.String       (IsString (..))
import           Text.Read

import           Verdict.Class
import           Verdict.Failure
import           Verdict.Logic
import           Verdict.Types

validate :: forall a base m.
           ( HaskVerdict (VPred a base) base,  Verdict a base, ApplicativeError ErrorTree m)
         => base -> m a
validate base = case haskVerdict (Proxy :: Proxy (VPred a base)) base of
    Nothing  -> pure $ unsafeCoerce base
    Just err -> throwError err
{-# RULES "validate/safe" validate = pure #-}

validateE :: ( HaskVerdict (VPred a base) base,  Verdict a base)
          => base -> Either ErrorTree a
validateE = validate

validateF :: ( HaskVerdict (VPred a base) base,  Verdict a base)
          => base -> Failure ErrorTree a
validateF = validate

-- | Coerce a value to another set of predicates if it's safe to do so (i.e.,
-- if the predicates of the original value imply the predicates of the return
-- type).
safeCoerce :: forall a1 a2 base. (Verdict a1 base, Verdict a2 base
            , VPred a1 base `Implies` VPred a2 base)
           => a1 -> a2
safeCoerce x = unsafeCoerce (unvalidate x :: base)


type family VPred a b where
    VPred (Validated c a) b = c :&& VPred a b
    VPred a             a   = ()
    VPred a             b   = VPred' a b

type family VPred' a b

------------------------------------------------------------------------------
-- * Validated
------------------------------------------------------------------------------
-- | A generalization of smart constructors with opaque types.
-- Construct a @Validated@ with 'validate'.
newtype Validated constraint a = Validated { getVal :: a }
    deriving (Show, Eq, Ord)


instance {-# OVERLAPPING #-}  Verdict (Validated c a) a

-- * Validated ()

instance Functor (Validated ()) where
    fmap = coerce

instance Applicative (Validated ()) where
    pure  = Validated
    (<*>) = coerce

instance Monad (Validated ()) where
    return = Validated
    m >>= k = k (getVal m)

instance MonadFix (Validated ()) where
    mfix f = Validated (fix (getVal . f))

instance MonadZip (Validated ()) where
    mzipWith = coerce
    munzip   = coerce

instance Monoid m => Monoid (Validated () m) where
    mempty       = Validated mempty
    mappend a b  = Validated $ mappend (getVal a) (getVal b)

instance Foldable (Validated ()) where
    foldMap                 = coerce
    elem                    = (. getVal) #. (==)
    foldl                   = coerce
    foldl'                  = coerce
    foldl1 _                = getVal
    foldr f z (Validated x) = f x z
    foldr'                  = foldr
    foldr1 _                = getVal
    length _                = 1
    maximum                 = getVal
    minimum                 = getVal
    null _                  = False
    product                 = getVal
    sum                     = getVal
    toList (Validated x)    = [x]

{-
instance (HaskVerdict c v, Read v) => Read (Validated c v) where
    readPrec = force . validate <$> readPrec
      where force = either (error . show) id

instance (HaskVerdict c v, IsString v) => IsString (Validated c v) where
    fromString = force . validate . fromString
      where force = either (error . show) id

protect :: ( ApplicativeError (String, ErrorTree) m
           , HaskVerdict c a
           ) => Proxy c -> String -> (a -> b) -> a -> m b
protect p name fn a = case haskVerdict p a of
    Nothing -> pure $ fn a
    Just e  -> throwError (name, e)

-- | Checks a non-'Validated' value against a set of constraints given by a
-- 'Proxy'.
checkWith :: forall m c a . (ApplicativeError ErrorTree m, HaskVerdict c a)
          => a -> Proxy c -> m a
checkWith v _ = getVal <$> v'
  where v' = validate v :: ApplicativeError ErrorTree m => m (Validated c a)

isValid :: forall c a . (HaskVerdict c a) => Proxy c -> a -> Bool
isValid v = isRight . (`checkWith` v)

-}

-- | Function composition. Typechecks if the result of applying the first
-- function has a constraint that implies the constraint of the argument of the
-- second function.
(|.) :: (cb' `Implies` cb)
    => (Validated cb b -> Validated cc c)
    -> (Validated ca a -> Validated cb' b)
    -> Validated ca a -> Validated cc c
f |. g = f . coerce . g
infixr 8 |.

-- * Internal
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
