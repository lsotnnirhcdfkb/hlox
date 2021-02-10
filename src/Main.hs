module Main where

import System.Environment
import System.IO
import System.Console.ParseArgs
import Frontend.Scan
import Frontend.Parse
import Frontend.Diagnostic
import Treewalk.Interpret
import Runtime.Value

data InterpreterSettings = InterpreterSettings
                           { errorFormat :: ErrorFormat
                           , backend :: Backend
                           , file :: Maybe String
                           }

data ErrorFormat = DefaultErrorFormat | OriginalErrorFormat
                 deriving Show
data Backend = Treewalk | StackBC | RegisterBC
             deriving Show

readInterpreterSettings :: Args String -> Either String InterpreterSettings
readInterpreterSettings args =
    let errorFormatStr = getRequiredArg args "errformat" :: String
        backendStr = getRequiredArg args "backend" :: String
        interpretingFile = getArg args "file" :: Maybe String

    in (case errorFormatStr of
            "default" -> Right DefaultErrorFormat
            "original" -> Left "original error format is not implemented (yet)"
            _ -> Left $ "invalid error format: '" ++ errorFormatStr ++ "'"
        ) >>= \errorFormat ->
        (case backendStr of
            "treewalk" -> Right Treewalk
            "stackbc" -> Left "stack bytecode interpreter is not implemented (yet)"
            "registerbc" -> Left "register bytecode interpreter is not implemented (yet)"
            _ -> Left $ "invalid backend: '" ++ backendStr ++ "'"
        ) >>= \backend ->
        Right InterpreterSettings
        { errorFormat = errorFormat
        , backend = backend
        , file = interpretingFile
        }

main :: IO ()
main =
    getProgName >>= \progName ->
    getArgs >>= \args ->
    let parsedArgs = parseArgs argsControl argsSpec progName args
        settings = readInterpreterSettings parsedArgs
    in case settings of
        Right set -> runInterpreter set
        Left err -> hPutStrLn stderr $ "hlox: " ++ err
    where
        argsControl = ArgsParseControl
                     { apcComplete = ArgsComplete
                     , apcDash = ArgsHardDash
                     }
        argsSpec = [ Arg
                     { argIndex = "errformat"
                     , argAbbr = Just 'e'
                     , argName = Just "error-format"
                     , argData = argDataDefaulted "format" ArgtypeString "default"
                     , argDesc = "the error format"
                     }
                   , Arg
                     { argIndex = "backend"
                     , argAbbr = Just 'b'
                     , argName = Just "backend"
                     , argData = argDataDefaulted "backend" ArgtypeString "treewalk"
                     , argDesc = "the backend to interpret with"
                     }
                   , Arg
                     { argIndex = "file"
                     , argAbbr = Nothing
                     , argName = Nothing
                     , argData = argDataOptional "file" ArgtypeString
                     , argDesc = "the file to interpret"
                     }
                   ]

runInterpreter :: InterpreterSettings -> IO ()
runInterpreter settings = case file settings of
    Just file -> runFile file
    Nothing -> repl

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

