{-# LANGUAGE UndecidableInstances #-}
module Verdict.JSON.Class where

import           Data.Monoid
import           Data.Proxy
import qualified Data.Text          as Text
import           Data.Void (Void)
import           GHC.TypeLits
import           Verdict
import           Verdict.JSON.Types

------------------------------------------------------------------------------
-- * JsonVerdict
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- * JsonType
------------------------------------------------------------------------------

class JsonType a where
    jsonType :: proxy a -> SchemaType

instance JsonType Integer where
    jsonType _ = IntegerT

instance JsonType Int where
    jsonType _ = IntegerT

instance JsonType Float where
    jsonType _ = NumberT

instance JsonType Double where
    jsonType _ = NumberT

instance JsonType String where
    jsonType _ = StringT

instance JsonType Text.Text where
    jsonType _ = StringT

instance JsonType Bool where
    jsonType _ = BooleanT

instance JsonType a => JsonType [a] where
    jsonType _ = ArrayT $ jsonType p
      where p = Proxy :: Proxy a

------------------------------------------------------------------------------
-- * JsonSchema
------------------------------------------------------------------------------
class JsonSchema a where
    jsonSchema :: proxy a -> AnySchema

instance (KnownNat n, Integral i) => JsonSchema (Validated (Maximum n) i) where
    jsonSchema _ = NumericS $ defNS { maximum = Just v }
      where v = fromInteger $ natVal (Proxy :: Proxy n)

{-instance JsonSchema a => JsonSchema (Maybe a) where-}
    {-jsonSchema _ = jsonSchema p { required = False }-}
      {-where p = Proxy :: Proxy a-}
