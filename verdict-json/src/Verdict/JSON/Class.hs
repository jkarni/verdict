{-# LANGUAGE UndecidableInstances #-}
module Verdict.JSON.Class where

import Data.Monoid
import qualified Data.Text as Text
import GHC.TypeLits
import Data.Proxy
import Verdict.JSON.Types
import Verdict

class JsonVerdict a where
    jsonVerdict :: proxy a -> [JsonConstraint Text.Text]

instance (KnownNat a) => JsonVerdict (Maximum a) where
    jsonVerdict _ = [Maximum (fromInteger $ natVal (Proxy :: Proxy a))]

instance (KnownNat a) => JsonVerdict (Minimum a) where
    jsonVerdict _ = [Minimum (fromInteger $ natVal (Proxy :: Proxy a))]

instance (KnownNat a) => JsonVerdict (MaxLength a) where
    jsonVerdict _ = [MaxLength (fromInteger $ natVal (Proxy :: Proxy a))]

instance (KnownNat a) => JsonVerdict (MinLength a) where
    jsonVerdict _ = [MinLength (fromInteger $ natVal (Proxy :: Proxy a))]

instance (JsonVerdict a, JsonVerdict b) => JsonVerdict (a :&& b) where
    jsonVerdict _ = jsonVerdict pa <> jsonVerdict pb
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

class JsonSchema a where
    jsonSchema :: proxy a -> Spec
