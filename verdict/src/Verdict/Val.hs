{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verdict.Val where

import           Data.Coerce     (coerce)
import           Data.Proxy
import           Data.String     (IsString (..))
import           Text.Read

import           Verdict.Class
import           Verdict.Failure
import           Verdict.Logic
import           Verdict.Types

------------------------------------------------------------------------------
-- * Val
------------------------------------------------------------------------------
-- | A generalization of smart constructors with opaque types.
-- Construct a @Validated@ with 'validate'.
newtype Validated constraint a = Validated { getVal :: a }
    deriving (Show, Eq, Ord)

instance (HaskVerdict c v, Read v) => Read (Validated c v) where
    readPrec = force . validate <$> readPrec
      where force = either (error . show) id

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

-- | Function composition. Typechecks if the result of applying the first
-- function has a constraint that implies the constraint of the argument of the
-- second function.
(|.) :: (cb' `Implies` cb)
    => (Validated cb b -> Validated cc c)
    -> (Validated ca a -> Validated cb' b)
    -> Validated ca a -> Validated cc c
f |. g = f . coerce . g
infixr 8 |.
