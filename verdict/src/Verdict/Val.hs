{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verdict.Val where

import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Coerce     (Coercible, coerce)
import           Data.Either (isRight)
import           Data.Foldable
import           Data.Proxy
import           Data.String     (IsString (..))
import           Text.Read

import           Verdict.Class
import           Verdict.Failure
import           Verdict.Logic
import           Verdict.Types

------------------------------------------------------------------------------
-- * Validated
------------------------------------------------------------------------------
-- | A generalization of smart constructors with opaque types.
-- Construct a @Validated@ with 'validate'.
newtype Validated constraint a = Validated { getVal :: a }
    deriving (Eq, Ord)

-- * Validated ()

-- @Validated ()@ is the same as 'Data.Functor.Identity'; we use the same
-- instances.
validateEmpty :: a -> Validated () a
validateEmpty = coerce

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

instance (HaskVerdict c v, Read v) => Read (Validated c v) where
    readPrec = force . validate <$> readPrec
      where force = either (error . show) id

instance Show v => Show (Validated c v) where
    show        (Validated v) = show v
    showsPrec i (Validated v) = showsPrec i v

instance (HaskVerdict c v, IsString v) => IsString (Validated c v) where
    fromString = force . validate . fromString
      where force = either (error . show) id

-- | Constructs a @Validated c a@ from an @a@ if @a@ matches the constraints;
-- throws an error with a description of precise constraints not satisfied
-- otherwise.
validate :: forall c a m . (HaskVerdict c a, ApplicativeError ErrorTree m)
    => a -> m (Validated c a)
validate a = case haskVerdict (Proxy :: Proxy c) a of
    Nothing -> pure $ Validated a
    Just err -> throwError err

-- | Coerce a 'Validated' to another set of constraints. This is safe with
-- respect to memory corruption, but loses the guarantee that the values
-- satisfy the predicates.
unsafeCoerceVal :: Validated c a -> Validated c' a
unsafeCoerceVal = coerce

-- | Safely coerce a 'Validated' to a set of constraints implied by the
-- original ones.
coerceVal :: (c1 `Implies` c2) => Validated c1 a -> Validated c2 a
coerceVal = coerce

-- | Don't really validate
unsafeValidated :: a -> Validated c a
unsafeValidated = Validated

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
