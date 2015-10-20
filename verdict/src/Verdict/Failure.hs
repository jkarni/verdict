module Verdict.Failure (Failure(..), ApplicativeError(..)) where

import GHC.Generics (Generic)
import Data.Monoid
import Data.Typeable (Typeable)
import qualified Control.Monad.Trans.Except as ExceptT
import Control.Exception (IOException, catch)

data Failure e a = Failure e | Success a
  deriving (Eq, Show, Functor, Generic, Typeable)

class Applicative m => ApplicativeError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance Monoid e => Applicative (Failure e) where
    pure                            = Success
    Failure msgs  <*> Failure msgs' = Failure (msgs <> msgs')
    Success _     <*> Failure msgs' = Failure msgs'
    Failure msgs' <*> Success _     = Failure msgs'
    Success f     <*> Success x     = Success (f x)

instance Monoid e => ApplicativeError e (Failure e) where
    throwError               = Failure
    catchError (Failure e) f = f e
    catchError s           _ = s

instance ApplicativeError e (Either e) where
    throwError            = Left
    catchError (Left e) f = f e
    catchError s        _ = s

instance ApplicativeError IOException IO where
    throwError            = ioError
    catchError            = catch

instance Monad m => ApplicativeError e (ExceptT.ExceptT e m) where
    throwError            = ExceptT.throwE
    catchError            = ExceptT.catchE
