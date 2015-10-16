module Verdict.JSON.Class where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Proxy
import Verdict.JSON.Types
import Verdict

class JsonVerdict a where
    jsonVerdict :: proxy a -> JsonConstraint Text.Text

instance (KnownVal a Int) => JsonVerdict (Maximum a) where
    jsonVerdict _ = Maximum (knownVal (Proxy :: Proxy a))

class JsonSchema a where
    jsonSchema :: proxy a -> Spec
