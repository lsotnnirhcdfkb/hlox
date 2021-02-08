module Main where

import System.Environment
import System.IO
import Frontend.Scan
import Frontend.Parse
import Frontend.Ast
import Frontend.Diagnostic
import Treewalk.Interpret

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
run source = reportErrors >> (putStrLn $ show treewalked)
    where
        (scanned, scanErrors') = scan source
        scanErrors = map toErr scanErrors'

        (parsed, parseErrors') = parse scanned
        parseErrors = map toErr parseErrors'

        treewalked = interpret <$> parsed

        totalErrors = scanErrors ++ parseErrors
        reportErrors = mapM_ report totalErrors

        success = length totalErrors == 0
        printAST (BinaryExpr (Located _ lhs) (Located _ op) (Located _ rhs)) = "(" ++ show op ++ " " ++ printAST lhs ++ " " ++ printAST rhs ++ ")"
        printAST (GroupingExpr (Located _ e)) = "(" ++ printAST e ++ ")"
        printAST (BoolExpr (Located _ b)) = show b
        printAST (NumberExpr (Located _ n)) = show n
        printAST (StringExpr (Located _ s)) = "\"" ++ s ++ "\""
        printAST (UnaryExpr (Located _ operator) (Located _ operand)) = "(" ++ show operator ++ " " ++ printAST operand ++ ")"
