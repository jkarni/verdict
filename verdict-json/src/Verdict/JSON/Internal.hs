{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Verdict.JSON.Internal where

import           Data.Aeson           (FromJSON (..), ToJSON (..))
import           Data.Monoid          ((<>))
import           Data.Proxy           (Proxy (Proxy))
import           Data.Swagger
import           Data.Swagger.Declare (DeclareT)
import           GHC.TypeLits         (KnownNat, natVal)
import           Verdict

import           Control.Lens

#include "overlapping-compat.h"

------------------------------------------------------------------------------
-- * ToSchema instances
------------------------------------------------------------------------------


instance (KnownNat n, ToSchema i, Num i)
    => ToSchema (Validated (Maximum n) i) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy i)
                         & mapped . _2 . schemaMaximum .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat n, ToSchema i, Num i)
    => ToSchema (Validated (Minimum n) i) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy i)
                         & mapped . _2 . schemaMinimum .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance ( ToSchema (Validated c a), ToSchema (Validated c' a)
         ) => ToSchema (Validated (c :&& c') a) where
    declareNamedSchema _ = do
      let pa = Proxy :: Proxy (Validated c a)
          pb = Proxy :: Proxy (Validated c' a)
      (name, sa) <- declareNamedSchema pa
      (_   , sb) <- declareNamedSchema pb
      return (name, sa <> sb)

instance OVERLAPPING_ (KnownNat n)
    => ToSchema (Validated (MaxLength n) String) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
                         & mapped . _2 . schemaMaxLength .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance OVERLAPPING_ (KnownNat n)
    => ToSchema (Validated (MinLength n) String) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
                         & mapped . _2 . schemaMinLength .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance OVERLAPPABLE_ (KnownNat n, ToSchema [a])
    => ToSchema (Validated (MaxLength n) [a]) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
                         & mapped . _2 . schemaMaxItems .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance OVERLAPPABLE_ (KnownNat n, ToSchema [a])
    => ToSchema (Validated (MinLength n) [a]) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
                         & mapped . _2 . schemaMinItems .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

------------------------------------------------------------------------------
-- * From/ToJSON instances
------------------------------------------------------------------------------

instance (HaskVerdict c a, FromJSON a) => FromJSON (Validated c a) where
    parseJSON x = parseJSON x >>= either (fail . show) return . validate

instance (ToJSON a) => ToJSON (Validated c a) where
    toJSON = toJSON . getVal

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

onSndSPS :: Setter' (DeclareT Definitions Identity NamedSchema) (ParamSchema Schema)
onSndSPS = mapped . _2 . schemaParamSchema
