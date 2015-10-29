module Verdict.QuickCheck where

import Data.Either
import Data.Proxy
import GHC.TypeLits
import Test.QuickCheck
import Verdict

-- | Default instance. Not very efficient, since values are generated
-- indiscriminately and then filtered.
instance (HaskVerdict c v, Arbitrary v) => Arbitrary (Validated c v) where
    arbitrary = unsafeValidated <$> (arbitrary `suchThat` isValid p)
      where p = Proxy :: Proxy c
    shrink v = rights $ validate <$> shrink (getVal v)
      where p = Proxy :: Proxy c

instance (Arbitrary (Validated c1 a), Arbitrary (Validated c2 a))
    => Arbitrary (Validated (c1 :|| c2) a) where
    arbitrary = oneof [coerceVal <$> g1, coerceVal <$> g2]
      where g1 = arbitrary :: Gen (Validated c1 a)
            g2 = arbitrary :: Gen (Validated c2 a)

