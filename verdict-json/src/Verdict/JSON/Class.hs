{-# LANGUAGE ScopedTypeVariables #-}
module Verdict.JSON.Class where

import           Data.Monoid
import           Data.Proxy
import           GHC.TypeLits
import           Verdict
import           Verdict.JSON.Types
import qualified Data.Map as Map
import Generics.Eot
import qualified Data.Text as Text

import Debug.Trace

------------------------------------------------------------------------------
-- * JsonSchema
------------------------------------------------------------------------------

class JsonSchema a where
    jsonSchema :: Proxy a -> AnySchema
    default jsonSchema :: (HasEot a, GJsonSchema (Eot a)) => Proxy a -> AnySchema
    jsonSchema p = ObjectS $ gjsonSchema p' (f, mempty)
      where
        p' = Proxy :: Proxy (Eot a)
        g (Selectors x) = x
        g _             = error "not implemented"
        f = g . fields <$> constructors (datatype p)

instance (KnownNat n, Num i) => JsonSchema (Validated (Maximum n) i) where
    jsonSchema _ = NumericS $ mempty { maximum' = Just $ Max v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat n, Num i) => JsonSchema (Validated (Minimum n) i) where
    jsonSchema _ = NumericS $ mempty { minimum' = Just $ Min v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance ( JsonSchema (Validated c a), JsonSchema (Validated c' a)
         ) => JsonSchema (Validated (c :&& c') a) where
    jsonSchema _ = jsonSchema pa <> jsonSchema pb
      where pa = Proxy :: Proxy (Validated c a)
            pb = Proxy :: Proxy (Validated c' a)

instance (KnownNat n) => JsonSchema (Validated (MaxLength n) String) where
    jsonSchema _ = StringS $ mempty { maxLength = Just $ Max v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance ( KnownNat n) => JsonSchema (Validated (MinLength n) String) where
    jsonSchema _ = StringS $ mempty { minLength = Just $ Min v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

------------------------------------------------------------------------------
-- * GJsonSchema
------------------------------------------------------------------------------

class GJsonSchema eot where
    gjsonSchema :: proxy eot
                -> ([[String]], ObjectSchema)  -- ^ accumulator
                -> ObjectSchema

instance GJsonSchema this => GJsonSchema (Either this Void) where
    gjsonSchema _ ([xs], o) = gjsonSchema (Proxy :: Proxy this) ([xs], o)

instance (JsonSchema x, GJsonSchema xs) => GJsonSchema (x, xs) where
    gjsonSchema _ ([n:ns], o) = gjsonSchema ps ([ns], o')
      where
        p = Proxy :: Proxy x
        ps = Proxy :: Proxy xs
        addProp s = Map.insert (Text.pack n) (Required, jsonSchema p) s
        o' = o { properties = addProp $ properties o }
    gjsonSchema _ ([[]], o)     = o
    gjsonSchema _ x             = error "impossible (hopefully)"

instance GJsonSchema () where
    gjsonSchema _ ([[]], o) = o

instance GJsonSchema Void where
    gjsonSchema _ _ = error "impossible"
