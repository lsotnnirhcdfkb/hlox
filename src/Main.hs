module Main where

import System.Environment
import System.IO
import Frontend.Scan
import Frontend.Parse
import Frontend.Diagnostic
import Treewalk.Interpret
import Runtime.Value

main :: IO ()
main = getArgs >>=
    \args -> case args of
        []      -> repl
        file:[] -> runFile file
        _       -> putStrLn "bad usage"

repl :: IO ()
repl = putStr "> " >> hFlush stdout >> getLine >>= run >> repl

runFile :: String -> IO ()
runFile fileName = readFile fileName >>= run

run :: String -> IO ()
run source =
    let (scanned, scanErrors') = scan source
        scanErrors = map toErr scanErrors'

        (parsed, parseErrors') = parse scanned
        parseErrors = map toErr parseErrors'

        beforeRunErrs = scanErrors ++ parseErrors
        reportBeforeRunErrs = mapM_ report beforeRunErrs
        beforeRunSuccess = length beforeRunErrs == 0

        treewalked = if beforeRunSuccess
        then case parsed of
                Just ast -> Just $ interpret ast
                _ -> error "parsed is Nothing but there are no before run errors"
        else Nothing
    in reportBeforeRunErrs >>
    case treewalked of
        Just (Right (Located _ res)) -> putStrLn $ stringifyValue res
        Just (Left runErr) -> report $ toErr runErr
        _ -> return ()

