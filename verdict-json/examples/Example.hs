{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
import qualified Data.Map     as Map
import           Data.Proxy   (Proxy (..))
import           GHC.Generics (Generic)

import           Verdict
import           Verdict.JSON

type NameC = MinLength 1 :&& MaxLength 100
type Name = Validated NameC String
type AgeC = Minimum 0 :&& Maximum 200
type Age  = Validated AgeC Integer

data Person = Person
    { name :: Name
    , age  :: Age
    } deriving (Eq, Show, Read, Generic)

instance JsonSchema Person where
    jsonSchema _ = JsonSpec $ Map.fromList [ ("name", Left $ jsonVerdict namep)
                                           , ("age" , Left $ jsonVerdict agep )
                                           ]
      where namep = Proxy :: Proxy NameC
            agep  = Proxy :: Proxy AgeC

main :: IO ()
main = print $ jsonSchema (Proxy :: Proxy Person)
