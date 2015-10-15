module Verdict.JSON.Class where

import qualified Data.Text as Text
import Verdict.JSON.Types

class JsonVerdict a where
    jsonVerdict :: proxy a -> JsonConstraint Text.Text

