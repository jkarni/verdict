{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Verdict.JSON
    ( JsonSchema(..)
    , ObjectSchema(..)
    , AnySchema(..)
    , Required(..)
    , mkAny
    , jsonSchema
    , deriveJSONSchema
    , Options(..)
    ) where

import Data.Aeson
import Verdict

import Verdict.JSON.Class
import Verdict.JSON.Types
import Verdict.JSON.TH

instance (HaskVerdict c a, FromJSON a) => FromJSON (Validated c a) where
    parseJSON x = parseJSON x >>= either (fail . show) return . validate

instance (ToJSON a) => ToJSON (Validated c a) where
    toJSON = toJSON . getVal
