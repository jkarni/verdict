{-# LANGUAGE ScopedTypeVariables #-}
module Verdict.JSON.Class where

import           Data.Monoid
import           Data.Proxy
import qualified Data.Text          as Text
import           GHC.TypeLits
import           Verdict
import           Verdict.JSON.Types

------------------------------------------------------------------------------
-- * JsonVerdict
------------------------------------------------------------------------------

class JsonVerdict a where
    jsonVerdict :: proxy a -> [JsonConstraint Text.Text]

instance (KnownNat a) => JsonVerdict (Maximum a) where
    jsonVerdict _ = [Maximum (fromInteger $ natVal (Proxy :: Proxy a))]

instance (KnownNat a) => JsonVerdict (Minimum a) where
    jsonVerdict _ = [Minimum (fromInteger $ natVal (Proxy :: Proxy a))]

instance (KnownNat a) => JsonVerdict (MaxLength a) where
    jsonVerdict _ = [MaxLength (fromInteger $ natVal (Proxy :: Proxy a))]

instance (KnownNat a) => JsonVerdict (MinLength a) where
    jsonVerdict _ = [MinLength (fromInteger $ natVal (Proxy :: Proxy a))]

instance (JsonVerdict a, JsonVerdict b) => JsonVerdict (a :&& b) where
    jsonVerdict _ = jsonVerdict pa <> jsonVerdict pb
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

------------------------------------------------------------------------------
-- * JsonSchema
------------------------------------------------------------------------------

class (MkAny (JsonType a), Monoid (JsonType a)) => JsonSchema a where
    type JsonType a
    jsonSchema' :: proxy a -> JsonType a

instance (KnownNat n)
         => JsonSchema (Validated (Maximum n) i) where
    type JsonType (Validated (Maximum n) i) = NumericSchema
    jsonSchema' _ = mempty { maximum' = Just $ Max v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat n)
         => JsonSchema (Validated (Minimum n) i) where
    type JsonType (Validated (Minimum n) i) = NumericSchema
    jsonSchema' _ = mempty { minimum' = Just $ Min v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance ( JsonSchema (Validated c a), JsonSchema (Validated c' a)
         , JsonType (Validated c a) ~ JsonType (Validated c' a)
         ) => JsonSchema (Validated (c :&& c') a) where
    type JsonType (Validated (c :&& c') a) = JsonType (Validated c a)
    jsonSchema' _ = jsonSchema' pa <> jsonSchema' pb
      where pa = Proxy :: Proxy (Validated c a)
            pb = Proxy :: Proxy (Validated c' a)

instance ( KnownNat n
         ) => JsonSchema (Validated (MaxLength n) String) where
    type JsonType (Validated (MaxLength n) String) = StringSchema
    jsonSchema' _ = mempty { maxLength = Just $ Max v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance ( KnownNat n
         ) => JsonSchema (Validated (MinLength n) String) where
    type JsonType (Validated (MinLength n) String) = StringSchema
    jsonSchema' _ = mempty { minLength = Just $ Min v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

class MkAny a where
    mkAny :: a -> AnySchema

instance MkAny ObjectSchema where
    mkAny = ObjectS

instance MkAny NumericSchema where
    mkAny = NumericS

instance MkAny StringSchema where
    mkAny = StringS

jsonSchema :: JsonSchema a => proxy a -> AnySchema
jsonSchema = mkAny . jsonSchema'
