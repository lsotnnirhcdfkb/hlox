module Main where

import System.Environment
import Scan
import Parse
import Diagnostic (report, toErr)

main :: IO ()
main = getArgs >>=
    \args -> case args of
        []      -> repl
        file:[] -> runFile file
        _       -> putStrLn "bad usage"

repl :: IO ()
repl = getLine >>= run >> repl

runFile :: String -> IO ()
runFile fileName = readFile fileName >>= run

run :: String -> IO ()
run source = reportErrors >> (putStrLn $ show parsed)
    where
        (scanned, scanErrors') = scan source
        scanErrors = map toErr scanErrors'

        (parsed, parseErrors') = parse scanned
        parseErrors = map toErr parseErrors'

        totalErrors = scanErrors ++ parseErrors
        reportErrors = mapM_ report totalErrors

        success = length totalErrors == 0
