# Verdict
~~~ {.haskell}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.TypeLits
import Data.Proxy
import Data.Monoid
import qualified Data.Text as Text
import Verdict
~~~

## A very general smart constructor

Sometimes you want to provide extra guarantees about a type that are best
expressed in terms of a smart constructor with an opaque type:


~~~ {.haskell}
newtype NonEmptyList' a = NonEmptyList' { getList :: [a] }
makeNonEmptyList :: [a] -> Maybe (NonEmptyList' a)
makeNonEmptyList x | null x    = Nothing
                   | otherwise = Just (NonEmptyList' x)
~~~

Where the `NonEmptyList` constructor is not exported.

This works, but it has quite a few downsides. You can no longer derive things
like `Read` and `FromJSON` without losing the invariants you set up. If you
have a `NonEmptyList` that is also of maximum length 100, you need to either
have a newtype to again wrap your `NonEmptyList`, or a newtype that expresses
both those constraints. Either way, all the functions you wrote for
`NonEmptyList`, and which ideally would work with `NonEmptyMax100List`, won't
do so (without some newtype unwrapping or coercing).

Instead, with `verdict` you can do this:

~~~ {.haskell}
type NonEmptyList a = Validated (Not (Length 0)) [a]
~~~

The smart constructor comes for free as `val`. Specialized for clarity:

~~~ {.haskell}
mkNonEmpty :: [a] -> Either ErrorTree (NonEmptyList a)
mkNonEmpty = val
~~~

The `Read` instance also comes for free, and checks for validity:

~~~ {.haskell}
testRead :: NonEmptyList Int
testRead = read "[]"
~~~

`testRead` will throw a descriptive error message.

## With some of the power of refinement types

Writing functions that express their precise assumptions can then be done with
`Implies`:

~~~ {.haskell}
safeHead :: (c `Implies` (Not (Length 0))) => Validated c [a] -> a
safeHead = head . getVal
~~~

Now, if we have another type:

~~~ {.haskell}
type ReasonableList a = Validated (Minimum 1 :&& Maximum 100) [a]
~~~

Our function `safeHead` will typecheck when given either a `ReasonableList` or
a `NonEmptyList` (or anything else with constraints that imply the length is
not zero).

The library provides some terms that can be used in the little constraint DSL.
Additionally, you may want to create your own. Doing so is as easy as declaring
an empty datatype, and a `HaskVerdict` instance for it. Let us for example
define a predicate `InCircle cx cy r`, which is true of a `Point` if the point
is inside a circle of radius `r` centered on coordinates `(cx, cy)`:

~~~ {.haskell}
data InCircle centerX centerY radius
data Point = Point { x :: Integer, y :: Integer } deriving (Eq, Show)

instance (KnownNat cx, KnownNat cy, KnownNat r)
  => HaskVerdict (InCircle cx cy r) Point where
   haskVerdict _
      = check (\p -> (x p - centerX) ^ 2 + (y p - centerY) ^ 2 < radius ^ 2) err
      where centerX = natVal (Proxy :: Proxy cx)
            centerY = natVal (Proxy :: Proxy cy)
            radius = natVal (Proxy :: Proxy r)
            err = "Point not inside circle: " <> (Text.pack $ show ((centerX, centerY), radius))
~~~

Additionally, you may also want to declare type instances for `Implies'` which
describe what things imply or are implied by your new constraint.

~~~ {.haskell}
type instance Implies' (InCircle cx cy r) (InCircle cx cy r') = r <= r'
~~~

## And opt-in validation capabilities

Sometimes you may not want to or be able to change the types of records. You
can still validate them:

~~~ {.haskell}
mkUpperRightQuad :: ApplicativeError ErrorTree m
                 => Integer -> Integer -> m Point
mkUpperRightQuad x' y' = Point <$> x' `checkWith` (Proxy :: Proxy (Minimum 0))
                               <*> y' `checkWith` (Proxy :: Proxy (Minimum 0))
~~~

If we instantiate `ApplicativeError` to a short-circuiting applicative (like
`Either`), only the first result will be returned. If we use something like
`Failure`, all of them will:

~~~ {.haskell}
main :: IO ()
main = do
  print (mkUpperRightQuad 5 3 :: Either ErrorTree Point)
  print (mkUpperRightQuad (-2) 1 :: Either ErrorTree Point)
  print (mkUpperRightQuad (-2) (- 1) :: Either ErrorTree Point)
  print (mkUpperRightQuad (-2) (- 1) :: Failure ErrorTree Point)
  print (mkUpperRightQuad 0 (- 1) :: Either ErrorTree Point)
  print testRead
~~~

The results:


    Right (Point {x = 5, y = 3})
    Left (Leaf "Should be more than 0")
    Left (Leaf "Should be more than 0")
    Failure (And (Leaf "Should be more than 0") (Leaf "Should be more than 0"))
    Left (Leaf "Should be more than 0")
    tutorial: Leaf "this still needs to be figured out"
