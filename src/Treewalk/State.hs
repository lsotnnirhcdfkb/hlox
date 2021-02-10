module Treewalk.State
    ( InterpreterState(..)
    , defineVar
    , getVar
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Runtime.Value

data InterpreterState = InterpreterState
                      { vars :: Map String LoxValue
                      }
                      deriving Show

defineVar :: InterpreterState -> String -> LoxValue -> InterpreterState
defineVar state name val = state
                           { vars = Map.insert name val $ vars state
                           }

getVar :: InterpreterState -> String -> Maybe LoxValue
getVar state name = Map.lookup name $ vars state
