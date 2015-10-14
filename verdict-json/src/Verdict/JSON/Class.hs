module Verdict.JSON.Class where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Verdict.JSON.Types

class JsonVerdict a where
    jsonVerdict :: Proxy a -> JsonConstraint Text.Text

