module Verdict.Generic where

import Generics.SOP

import Verdict.Class
import Verdict.Failure
import Verdict.Types

gValidate :: ( Generic b, All2 Validatable (Code b)
             , ApplicativeError ErrorTree m)
             => b' -> m b
gValidate = _

gBaseMap :: (AllN (Prod h) Validatable xs, HAp h) => h Base xs -> h I xs
gBaseMap = hcliftA (Proxy :: Proxy Validatable) (I . validate)
