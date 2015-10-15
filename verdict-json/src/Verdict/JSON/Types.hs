module Verdict.JSON.Types where

data JsonConstraint a
    = Minimum Int
    | Maximum Int
    | MaxLength Int
    | MinLength Int
    | OtherError a
    deriving (Eq, Show, Read, Functor)

