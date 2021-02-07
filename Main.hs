module Main where

import System.Environment
import Scan
import Parse

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
        (scanned, errors) = scan source
        reportErrors = mapM_ (putStrLn . show) errors
        scanSuccess = length errors == 0

        parsed = parse scanned
