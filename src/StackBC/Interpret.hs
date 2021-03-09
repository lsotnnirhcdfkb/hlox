module StackBC.Interpret where

import Frontend.Ast
import Frontend.Diagnostic
import Runtime.Error

interpret :: [Located Stmt] -> IO (Either RuntimeError ())
interpret = error "todo"
