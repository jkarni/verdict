{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Verdict.JSON.Internal where

import           Data.Monoid
import           Data.Proxy
import           Data.Swagger
import           Data.Swagger.Declare (DeclareT)
import           GHC.TypeLits
import           Verdict

import           Control.Lens

#include "overlapping-compat.h"

------------------------------------------------------------------------------
-- * ToSchema instances
------------------------------------------------------------------------------

onSndSPS :: Setter' (DeclareT Definitions Identity NamedSchema) (ParamSchema Schema)
onSndSPS = mapped . _2 . schemaParamSchema

instance (KnownNat n, ToSchema i, Num i) => ToSchema (Validated (Maximum n) i) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy i)
                         & onSndSPS . paramSchemaMaximum .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat n, ToSchema i, Num i) => ToSchema (Validated (Minimum n) i) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy i)
                         & onSndSPS . paramSchemaMinimum .~ Just v
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
                         & onSndSPS . paramSchemaMaxLength .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance OVERLAPPING_ (KnownNat n)
    => ToSchema (Validated (MinLength n) String) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
                         & onSndSPS . paramSchemaMinLength .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance OVERLAPPABLE_ (KnownNat n, ToSchema [a])
    => ToSchema (Validated (MaxLength n) [a]) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
                         & onSndSPS . paramSchemaMaxItems .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)

instance OVERLAPPABLE_ (KnownNat n, ToSchema [a])
    => ToSchema (Validated (MinLength n) [a]) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
                         & onSndSPS . paramSchemaMinItems .~ Just v
      where v = fromInteger $ natVal (Proxy :: Proxy n)
