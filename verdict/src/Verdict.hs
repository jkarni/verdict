module Verdict
    (

    -- * Essentials
      Validated
    , validate
    , getVal
    , coerceVal
    , unsafeCoerceVal
    , unsafeValidated
    , validateEmpty
    , check
    , checkWith
    , isValid
    , HaskVerdict(..)
    , Verdict(..)
    , GVerdict(..)
    , Implies
    , Implies'
    , KnownVal(..)
    , (|.)

    -- * Verdict Terms
    , (:&&)
    , (:||)
    , Not
    , Minimum
    , Maximum
    , MaxLength
    , MinLength
    , Length
    , MultipleOf
    , HasElem

    -- * Errors
    , ErrorTree
    , ErrorTree'(..)
    , Failure(..)
    , ApplicativeError(..)
    ) where

import           Verdict.Class
import           Verdict.Failure
import           Verdict.Logic
import           Verdict.Types
import           Verdict.Val
