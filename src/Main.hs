module Main where

import System.Environment
import System.IO
import System.Console.ParseArgs
import qualified Interpreter

readInterpreterSettings :: Args String -> Either String Interpreter.InterpreterSettings
readInterpreterSettings args =
    let backendStr = getRequiredArg args "backend" :: String
        interpretingFile = getArg args "file" :: Maybe String

    in (case backendStr of
            "treewalk" -> Right Interpreter.Treewalk
            "stackbc" -> Left "stack bytecode interpreter is not implemented (yet)"
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
