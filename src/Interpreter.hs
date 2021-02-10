module Interpreter
    ( interpret
    , InterpreterSettings(..)
    , Backend(..)
    ) where

import System.IO

import Frontend.Scan
import Frontend.Parse
import Frontend.Diagnostic
import qualified Treewalk.Interpret
import Runtime.Value

data InterpreterSettings = InterpreterSettings
                           { backend :: Backend
                           , file :: Maybe String
                           }

data Backend = Treewalk | StackBC | RegisterBC
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

        treewalked = if beforeRunSuccess
        then case parsed of
                Just ast -> Just $ Treewalk.Interpret.interpret ast
                _ -> error "parsed is Nothing but there are no before run errors"
        else Nothing
    in reportBeforeRunErrs >>
    case treewalked of
        Just (Right (Located _ res)) -> putStrLn $ stringifyValue res
        Just (Left runErr) -> report $ toErr runErr
        _ -> return ()

