{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Verdict.DB.Internal where

import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Map as Map
import GHC.TypeLits
import Verdict

------------------------------------------------------------------------------
-- API
empty :: MkIxs cs val => DB cs val
empty = DB { dbData = V.empty , dbIxs = mkIxs }

query :: forall c cs val. (DBVerdictIx c val, HOccurs c cs val) => DB cs val -> [Validated c val]
query db = query' ix (dbData db)
  where
    ix = hOccurrence (Proxy :: Proxy c) (dbIxs db)

insert :: (InsertAll cs val) => val -> DB cs val -> DB cs val
insert val db = DB { dbData = V.snoc (dbData db) val
                   , dbIxs = insertAll (V.length (dbData db), val) (dbIxs db)
                   }

data DB cs val = DB
    { dbData :: V.Vector val
    , dbIxs  :: HList cs val
    }

-- Polykinded tuple proxy
data Tup a b = Tup
------------------------------------------------------------------------------
-- DBVerdictIx
--
-- TODO: Find a better data structure
--
class DBVerdictIx (c :: k) val where
    type Index c val
    empty' :: Tup c val -> Index c val
    insert' :: Tup c val -> (Int, val) -> Index c val -> Index c val
    query' :: Index c val -> V.Vector val -> [Validated c val]

-- @*@-kinded (predicate) constraints.
instance (HaskVerdict c v) => DBVerdictIx (c :: *) v where
    type Index c v = ([Int], [Int])
    empty' _  = ([], [])
    insert' _ (i,val) (ts, fs) = if isValid p val then (i:ts, fs) else (ts, i:fs)
      where p = Proxy :: Proxy c
    query' (ts, fs) vec = [ unsafeValidated (vec V.! i) | i <- ts ]

-- @* -> *@-kinded (map) constraints.
instance ( HaskVerdict (c n) v, GetIxVal c v, GetIxValType c v ~ n, Ord n
         ) => DBVerdictIx (c :: * -> *) v where
    type Index c v = Map.Map (GetIxValType c v) Int
    empty' _ = Map.empty
    insert' _ (i,val) = Map.insert (getIxVal p val) i
      where p = Proxy :: Proxy c
    {-query' m vec =-}
------------------------------------------------------------------------------
-- GetIxVal
--
-- Gets the value to use as index for * -> * indices

class GetIxVal c val where
    type GetIxValType c val
    getIxVal :: Proxy c -> val -> GetIxValType c val

instance (Foldable f) => GetIxVal Length (f b) where
    type GetIxValType Length (f b) = Int
    getIxVal _ = length

------------------------------------------------------------------------------
-- InsertAll
--
-- Inserts across all indices
class InsertAll cs v where
    insertAll :: (Int, v) -> HList cs v -> HList cs v

instance InsertAll '[] v where
    insertAll _ HNil = HNil

instance (DBVerdictIx c v, InsertAll cs v) => InsertAll (c ': cs) v where
    insertAll new (HCons i rest) = HCons (insert' p new i) (insertAll new rest)
      where p = Tup :: Tup c v

------------------------------------------------------------------------------
-- MkIxs
--
-- Creates the appropriate empty indices
class MkIxs cs val where
    mkIxs :: HList cs val

instance MkIxs '[] val where
    mkIxs = HNil

instance (DBVerdictIx x val, MkIxs xs val) => MkIxs (x ': xs) val where
    mkIxs = HCons (empty' p) mkIxs
      where p = Tup :: Tup x val

------------------------------------------------------------------------------
-- HOccurs
--
-- Gets the first occurrence of a 'c'-index in the HList.
class HOccurs c cs v where
    hOccurrence :: Proxy c -> HList cs v -> Index c v

instance HOccurs x (x ': xs) v where
    hOccurrence _ (HCons i _) = i

instance (HOccurs x xs v) => HOccurs x (y ': xs) v where
    hOccurrence p (HCons i xs) = hOccurrence p xs

data HList xs v where
    HNil :: HList '[] v
    HCons :: Index c v -> HList cs v -> HList (c ': cs) v
