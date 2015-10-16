{-# LANGUAGE ConstraintKinds #-}
module Verdict.Val where

import Data.Proxy
import Data.Coerce
import Control.Arrow (first)
import Control.Monad.Error.Class
import Text.Read

import Verdict.Class
import Verdict.Types
import Verdict.Logic

------------------------------------------------------------------------------
-- * Val
------------------------------------------------------------------------------
-- The validated constructor is not exported
newtype Validated constraint a = Validated { getVal :: a }
    deriving (Show, Eq, Ord)

instance (HaskVerdict c v, Read v) => Read (Validated c v) where
    readPrec = force . val <$> readPrec
      where force = either (error . show) id

val :: forall c a m . (HaskVerdict c a, MonadError (ErrorTree String) m)
    => a -> m (Validated c a)
val a = case haskVerdict (Proxy :: Proxy c) a of
    Nothing -> return $ Validated a
    Just err -> throwError err

-- | Coerce a 'Validated' to another set of constraints. This is safe with
-- respect to memory corruption, but loses the guarantee that the values
-- satisfy the predicates.
unsafeCoerceVal :: Validated c a -> Validated c' a
unsafeCoerceVal = coerce

protect :: ( MonadError (String, ErrorTree String) m
        , HaskVerdict c a
        ) => Proxy c -> String -> (a -> b) -> a -> m b
protect p name fn a = case haskVerdict p a of
    Nothing -> return $ fn a
    Just e  -> throwError (name, e)

-- | Function composition. Typechecks if the result of applying the first
-- function has a constraint that implies the constraint of the argument of the
-- second function.
(|.) :: (cb' `Implies` cb)
    => (Validated cb b -> Validated cc c)
    -> (Validated ca a -> Validated cb' b)
    -> Validated ca a -> Validated cc c
f |. g = f . coerce . g
infixr 8 |.
