module Verdict.Val where

import Data.Proxy
import Data.Coerce

import Verdict.Class
import Verdict.Types
import Verdict.Logic

------------------------------------------------------------------------------
-- * Val
------------------------------------------------------------------------------
-- The validated constructor is not exported
newtype Validated constraint a = Validated { getVal :: a } deriving (Show, Eq)

val :: forall c a . HaskVerdict c a => a -> Either Errors (Validated c a)
val a = case haskVerdict (Proxy :: Proxy c) a of
    Nothing -> Right $ Validated a
    Just err -> Left err

-- | Function composition. Typechecks if the result of applying the first
-- function has a constraint that implies the constraint of the argument of the
-- second function.
(|.) :: (cb' `Implies` cb)
    => (Validated cb b -> Validated cc c)
    -> (Validated ca a -> Validated cb' b)
    -> Validated ca a -> Validated cc c
f |. g = f . coerce . g
infixr 8 |.
