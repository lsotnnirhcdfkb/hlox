module Interpreter
    ( interpret
    , InterpreterSettings(..)
    , Backend(..)
    ) where

import System.IO

import Frontend.Scan
import Frontend.Parse
import Frontend.Ast
import Frontend.Diagnostic
import qualified Treewalk.Interpret
import Runtime.Error

data InterpreterSettings = InterpreterSettings
                           { backend :: Backend
                           , file :: Maybe String
                           }

data Backend = Treewalk
             deriving Show

interpret :: InterpreterSettings -> IO ()
interpret settings = case file settings of
    Just file -> runFile settings file
    Nothing -> repl settings

repl :: InterpreterSettings -> IO ()
repl settings = putStr "> " >> hFlush stdout >> getLine >>= run settings >> repl settings

runFile :: InterpreterSettings -> String -> IO ()
runFile settings fileName = readFile fileName >>= run settings

run :: InterpreterSettings -> String -> IO ()
run settings source =
    let (scanned, scanErrors') = scan source
        scanErrors = map toErr scanErrors'

        (parsed, parseErrors') = parse scanned
        parseErrors = map toErr parseErrors'

        beforeRunErrs = scanErrors ++ parseErrors
        reportBeforeRunErrs = mapM_ report beforeRunErrs
        beforeRunSuccess = length beforeRunErrs == 0

        -- TODO: reuse interpreter state for repl
        backendFunction = chooseBackendFunction $ backend settings

        interpreted = if beforeRunSuccess
        then Just $ backendFunction parsed
        else Nothing

    in reportBeforeRunErrs >>
    case interpreted of
        Just res -> res >>= \ei ->
            case ei of
                Left runErr -> report $ toErr runErr
                Right () -> return ()
        _ -> return ()

chooseBackendFunction :: Backend -> ([Located Stmt] -> IO (Either RuntimeError ()))
chooseBackendFunction Treewalk = Treewalk.Interpret.interpretScript

