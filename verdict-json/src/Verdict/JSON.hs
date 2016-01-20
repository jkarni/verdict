{-# OPTIONS_GHC -fno-warn-orphans #-}
module Verdict.JSON () where

import Data.Aeson
import Verdict

import Verdict.JSON.Internal ()

instance (HaskVerdict c a, FromJSON a) => FromJSON (Validated c a) where
    parseJSON x = parseJSON x >>= either (fail . show) return . validate

instance (ToJSON a) => ToJSON (Validated c a) where
    toJSON = toJSON . getVal
