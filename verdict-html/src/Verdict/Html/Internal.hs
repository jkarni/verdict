module Verdict.Html.Internal where

import Control.Arrow (left)
import Control.Monad
import Web.HttpApiData
import qualified Data.Text as Text
import Verdict

instance (ToHttpApiData a) => ToHttpApiData (Validated c a) where
    toUrlPiece   = toUrlPiece . getVal
    toHeader     = toHeader . getVal
    toQueryParam = toQueryParam . getVal

instance (FromHttpApiData a, HaskVerdict c a) => FromHttpApiData (Validated c a) where
    parseUrlPiece   = parseUrlPiece >=> validateTextErr
    parseHeader     = parseHeader >=> validateTextErr
    parseQueryParam = parseQueryParam >=> validateTextErr

validateTextErr :: HaskVerdict c a => a -> Either Text.Text (Validated c a)
validateTextErr = left (Text.pack . show) . validate
