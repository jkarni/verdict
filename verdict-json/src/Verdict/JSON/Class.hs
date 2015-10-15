module Verdict.JSON.Class where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Verdict.JSON.Types

class JsonVerdict a where
    jsonVerdict :: proxy a -> JsonConstraint Text.Text

class JsonSchema a where
    jsonSchema :: proxy a -> Spec
