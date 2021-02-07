module Main where

import System.Environment
import Scan
import Parse
import Diagnostic

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
run source = errorsReported >> (putStrLn $ show parsed)
    where
        scanned = scan source
        scannedFiltered = [copyLocation token loc | loc@(Located { value = (Right token) }) <- scanned]

        errorsReported = sequence [putStrLn err | Located { value = (Left err) } <- scanned]

        parsed = parse scannedFiltered
