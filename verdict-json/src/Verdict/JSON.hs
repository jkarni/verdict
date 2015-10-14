module Verdict.JSON where

import Data.Aeson
import Verdict

instance (HaskVerdict c a, FromJSON a) => FromJSON (Validated c a) where
    parseJSON x = parseJSON x >>= either (fail . concat) return . val
