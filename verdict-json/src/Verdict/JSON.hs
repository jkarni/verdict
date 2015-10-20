{-# OPTIONS_GHC -fno-warn-orphans #-}
module Verdict.JSON
    ( JsonVerdict(..)
    , JsonSchema(..)
    , JsonSpec(..)
    , JsonConstraint(..)
    , SchemaType(..)
    , JsonType(..)
    ) where

import Data.Aeson
import Verdict

import Verdict.JSON.Class
import Verdict.JSON.Types

instance (HaskVerdict c a, FromJSON a) => FromJSON (Validated c a) where
    parseJSON x = parseJSON x >>= either (fail . show) return . val

instance (ToJSON a) => ToJSON (Validated c a) where
    toJSON = toJSON . getVal
