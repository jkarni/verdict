module Verdict.JSON.Generic where

import Verdict.JSON.Types
import Generics.SOP


gJsonSchema' :: (Generic a, HasDatatypeInfo a, All2 JsonSchema (Code a))
             => proxy a -> JsonType a
gJsonSchema' p = typeInfo p

gJsonSchema'' (All2 JsonSchema xs, Singl xs) => TypeInfo xs ->
gJsonSchema'' (ADT _ _ c)     =
gJsonSchema'' (Newtype _ _ c) =

gCon :: ConstructorInfo xs ->
gCon (Constructor cname) =
gCon (Record _ finfo) =
gCon (Infix _ _ _) =

