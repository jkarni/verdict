> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE OverloadedStrings #-}
> import Verdict
> import GHC.TypeLits
> import Data.Monoid ((<>))
> import qualified Data.Text as T
> import Data.Proxy


From Kiselyov (of course)

> data Z
> data S a
> class ToInt a where toInt :: proxy a -> Int
> instance ToInt Z where toInt _ = 0
> instance ToInt n => ToInt (S n) where toInt _ = 1 + toInt (Proxy :: Proxy n)
> class Sum2 a b c | a b -> c, a c -> b
> instance Sum2 Z a a
> instance Sum2 a b c => Sum2 (S a) b (S c)
> class Sum a b c | a b -> c, a c -> b, b c -> a
> instance (Sum2 a b c, Sum2 b a c) => Sum a b c
>
> instance HaskVerdict Z Int where
>   haskVerdict _ = check (== 0) ("Not zero")
> instance (ToInt n) => HaskVerdict (S n) Int where
>   haskVerdict _ = check (== p) ("Not equal to" <> T.pack p)
>    where p = toInt $ Proxy :: Proxy (S n)
>
> type VInt c = Validated c Int
> type Three = S (S (S Z))
> type Five = S (S Three)
> add :: Sum c1 c2 c3 => VInt c1 -> VInt c2 -> VInt c3
> add x y = unsafeValidated $ getVal x + getVal y

> main = do
>   x <- readLn
>   let v = add x (read "3" :: VInt Three) :: VInt Five
>   print v
