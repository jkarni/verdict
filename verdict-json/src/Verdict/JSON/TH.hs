{-# LANGUAGE TemplateHaskell #-}
module Verdict.JSON.TH where

import Data.Proxy
import Control.Monad
import qualified Data.Map as Map
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Verdict.JSON.Types
import Verdict.JSON.Class

data Options

deriveJSONSchema :: Options -> Name -> Q [Dec]
deriveJSONSchema opts name = do
    qR <- qReify name
    case qR of
        TyConI d -> (: []) <$> mkFor d
        _        -> error "Expected type constructor"

mkFor :: Dec -> Q Dec
-- mkFor (NewtypeD ctx n bndrs con name) =
mkFor (DataD _ctx n _bndrs [con] names) = mkInstance typ typSyn =<< props
  where
    typ    = ConT n
    typSyn = TySynEqn [typ] (ConT 'ObjectSchema)
    props  = mkPropsForCon con
mkFor (DataD {}) = error "Not implemented"

mkPropsForCon :: Con -> Q Exp
mkPropsForCon (RecC name recs) = ListE <$> forM recs go
  where
    go (name, strict, typ) = [| ( $n, (Required, jsonSchema (Proxy :: Proxy $t ))) |]
      where
        n = return . LitE . StringL $ show name
        t = return typ

mkInstance :: Type -> TySynEqn -> Exp -> Q Dec
mkInstance typ typSyn props = do
    m <- [| const $ mempty { properties = Map.fromList $(return props) } |]
    let methodI = ValD (VarP 'jsonSchema') (NormalB m) []
        typSynI = TySynInstD ''JsonType typSyn
    return $ InstanceD [] typ [typSynI, methodI]
