{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Verdict.JSON.Class where

import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as Text
import           Data.Proxy
import           GHC.TypeLits
import           Verdict
import           Verdict.JSON.Types


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

------------------------------------------------------------------------------
-- * GJsonSchema
------------------------------------------------------------------------------

class (MkAny (JsonType a), Monoid (JsonType a)) => GJsonSchema a where
    type GJsonType a
    gjsonSchema' :: proxy a -> GJsonType a

{-instance GJsonSchema (TRec x) where-}
    {-type GJsonType (TRec x) = ObjectSchema-}
    {-gjsonSchema' _ = mempty { properties-}

{-type JsonTypeFromPred a where-}
    {-JsonTypeFromPred (TRec x) = ObjectSchema-}

class TRecProps (a :: [k]) where
    tRecProps :: Proxy a -> Map.Map Text.Text (Required, AnySchema)

instance TRecProps '[] where
    tRecProps _ = Map.empty

instance (KnownSymbol name, JsonSchema c, TRecProps xs)
    => TRecProps (Rec name c ': xs) where
    tRecProps _ = Map.insert name (Required, jsonSchema pc) $ tRecProps pxs
      where
        name = Text.pack $ symbolVal (Proxy :: Proxy name)
        pc   = Proxy :: Proxy c
        pxs  = Proxy :: Proxy xs
