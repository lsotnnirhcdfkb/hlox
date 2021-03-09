module Main where

import System.Environment
import System.IO
import System.Console.ParseArgs
import qualified Interpreter

readInterpreterSettings :: Args String -> Either String Interpreter.InterpreterSettings
readInterpreterSettings args =
    let backendStr = getRequiredArg args "backend" :: String
        interpretingFile = getRequiredArg args "file" :: String

    in (case backendStr of
            "treewalk" -> Left "treewalk interpreter was removed"
            "stackbc" -> Right Interpreter.StackBC
            "registerbc" -> Left "register bytecode interpreter is not implemented (yet)"
            _ -> Left $ "invalid backend: '" ++ backendStr ++ "'"
        ) >>= \backend ->
        Right Interpreter.InterpreterSettings
        { Interpreter.backend = backend
        , Interpreter.file = interpretingFile
        }

main :: IO ()
main =
    getProgName >>= \progName ->
    getArgs >>= \args ->
    let parsedArgs = parseArgs argsControl argsSpec progName args
        settings = readInterpreterSettings parsedArgs
    in case settings of
        Right set -> Interpreter.interpret set
        Left err -> hPutStrLn stderr $ "hlox: " ++ err
    where
        argsControl = ArgsParseControl
                     { apcComplete = ArgsComplete
                     , apcDash = ArgsHardDash
                     }
        argsSpec = [ Arg
                     { argIndex = "backend"
                     , argAbbr = Just 'b'
                     , argName = Just "backend"
                     , argData = argDataDefaulted "backend" ArgtypeString "stackbc"
                     , argDesc = "the backend to interpret with"
                     }
                   , Arg
                     { argIndex = "file"
                     , argAbbr = Nothing
                     , argName = Nothing
                     , argData = argDataRequired "file" ArgtypeString
                     , argDesc = "the file to interpret"
                     }
                   ]
