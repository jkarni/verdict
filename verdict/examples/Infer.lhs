> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE OverloadedStrings #-}
> import Verdict


From Kiselyov (of course)

> class Sum2 a b c | a b -> c, a c -> b
> instance Sum2 Z a a
> instance Sum2 a b c => Sum2 (S a) b (S c)
> class Sum a b c | a b -> c, a c -> b, b c -> a
> instance (Sum2 a b c, Sum2 b a c) => Sum a b c
>
> type VInt c = Validated (Equals c) Int
> type Three = S (S (S Z))
> type Five = S (S Three)
> add :: Sum c1 c2 c3 => VInt c1 -> VInt c2 -> VInt c3
> add x y = unsafeValidated $ getVal x + getVal y

> main :: IO ()
> main = do
>   x <- readLn
>   let v = add x (read "3" :: VInt Three) :: VInt Five
>   print v
