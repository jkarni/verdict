{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
module Verdict.DB.Internal where

import Data.Proxy
import qualified Data.Vector as V
import GHC.TypeLits
import Verdict

data VerdictDB a b

-- TODO: Find a better data structure
--
-- | A single secondary key index
class DBVerdict c val where
    type Index c val
    empty :: Index c val
    insert :: (Int, val) -> Index c val -> Index c val
    query :: Index c val -> V.Vector val -> [Validated c val]

instance (HaskVerdict c v) => DBVerdict c v where
    type Index c v = ([Int], [Int])
    empty = ([], [])
    insert (i,val) (ts, fs) = if isValid p val then (i:ts, fs) else (ts, i:fs)
      where p = Proxy :: Proxy c
    query (ts, fs) vec = [ unsafeValidated (vec V.! i) | i <- ts ]

-- Gets the first occurrence of a 'c'-index in the HList.
class HOccurs c cs v where
    hOccurrence :: Proxy c -> HList cs v -> Index c v

instance HOccurs x (x ': xs) v where
    hOccurrence _ (HCons i _) = i

instance (HOccurs x xs v) => HOccurs x (y ': xs) v where
    hOccurrence p (HCons i xs) = hOccurrence p xs

data HList xs v where
    HNil :: HList '[] v
    HCons :: DBVerdict c v => Index c v -> HList cs v -> HList (c ': cs) v
{-
instance (DBVerdict c v, DBVerdict cs v) => DB (c ': cs) v where
    type Index (c ': cs) v = (Index c v, Index cs v)
    empty = empty : empty
    insert new ixs = insert new <$> ixs
-}
