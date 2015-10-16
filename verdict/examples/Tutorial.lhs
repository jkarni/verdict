> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeOperators #-}
> import Verdict

Sometimes you want to provide extra guarantees about a type that are best
expressed in terms of a smart constructor with an opaque type:


> newtype NonEmptyList' a = NonEmptyList' { getList :: [a] }
> makeNonEmptyList :: [a] -> Maybe (NonEmptyList' a)
> makeNonEmptyList x | null x    = Nothing
>                    | otherwise = Just (NonEmptyList' x)

Where the 'NonEmptyList' constructor is not exported.

This works, but it has quite a few downsides. You can no longer derive things
like 'Read' and 'FromJSON' without losing the invariants you set up. If you
have a 'NonEmptyList' that is also of maximum length 100, you need to either
have a newtype to again wrap your 'NonEmptyList', or a newtype that expresses
both those constraints. Either way, all the functions you wrote for
'NonEmptyList', and which ideally would work with 'NonEmptyMax100List', won't
do so (without some newtype unwrapping or coercing).

Instead, with 'verdict' you can do this:

> type NonEmptyList a = Validated (Not (Length 0)) [a]

The smart constructor comes for free as 'val'. Specialized for clarity:

> mkNonEmpty :: [a] -> Either (ErrorTree String) (NonEmptyList a)
> mkNonEmpty = val

The 'Read' instance also comes for free, and checks for validity:

> testRead :: NonEmptyList Int
> testRead = read "[]"

'testRead' will throw a descriptive error message.

Writing functions that express their precise assumptions can then be done with
'Implies':

> safeHead :: (c `Implies` (Not (Length 0))) => Validated c [a] -> a
> safeHead = head . getVal

Now, if we have another type:

> type ReasonableList a = Validated (Minimum 1 :&& Maximum 100) [a]

Our function 'safeHead' will typecheck when given either a 'ReasonableList' or
a 'NonEmptyList' (or anything else with constraints that imply the length is
not zero).

The library provides some terms that can be used in the little constraint DSL.
Additionally, you may want to create your own. Doing so is as easy as declaring
an empty datatype, and a 'HaskVerdict' instance for it. Additionally, you may
also want to declare type instances for 'Implies'' which describe what things
imply or are implied by your new constraint.

-- TODO: Json schema, form validation

> main :: IO ()
> main = print $ testRead
