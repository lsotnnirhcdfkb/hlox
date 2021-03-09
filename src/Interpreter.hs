module Interpreter
    ( interpret
    , InterpreterSettings(..)
    , Backend(..)
    ) where

import Frontend.Scan
import Frontend.Parse
import Frontend.Ast
import Frontend.Diagnostic
import Runtime.Error

import qualified StackBC.Interpret

data InterpreterSettings = InterpreterSettings
                           { backend :: Backend
                           , file :: String
                           }

data Backend = StackBC
             deriving Show

interpret :: InterpreterSettings -> IO ()
interpret settings = readFile (file settings) >>= run settings

runStage :: LoxError b => (a, [b]) -> (a, [Error])
runStage (result, stageErrors) = (result, map toErr stageErrors)

joinStages :: (LoxError b, LoxError d) => (a, [b]) -> (a -> (c, [d])) -> (c, [Error])
firstRes `joinStages` nextStage =
    let (firstStageRes, firstStageErrs) = runStage firstRes
        (secondStageRes, secondStageErrs) = runStage $ nextStage firstStageRes
    in (secondStageRes, firstStageErrs ++ secondStageErrs)

run :: InterpreterSettings -> String -> IO ()
run settings source =
    let (beforeRunRes, beforeRunErrs) =
            (scan source) `joinStages` parse
        backendFunction = chooseBackendFunction $ backend settings
    in case beforeRunErrs of
        [] ->
            backendFunction beforeRunRes >>= \runRes ->
            case runRes of
                Left runErr -> report $ toErr runErr
                Right () -> return ()
        errs@(_:_) -> mapM_ report errs

chooseBackendFunction :: Backend -> ([Located Stmt] -> IO (Either RuntimeError ()))
chooseBackendFunction StackBC = StackBC.Interpret.interpret

