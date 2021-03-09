module Treewalk.Environment
    ( Environemnt(..)
    , defineVar
    , getVar
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Runtime.Value

data Environment = Environment
    { vars :: Map String LoxValue
    }

defineVar :: Environment -> String -> LoxValue -> Environment
defineVar state name val = state
                           { vars = Map.insert name val $ vars state
                           }

getVar :: Environment -> String -> Maybe LoxValue
getVar state name = Map.lookup name $ vars state
