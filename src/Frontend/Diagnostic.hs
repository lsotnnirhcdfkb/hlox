module Frontend.Diagnostic
    ( Located(..)
    , Span(..)
    , DescriptiveLocation(..)
    , joinSpan
    , Error(..)
    , Message(..)
    , report
    , ToError(toErr)
    ) where

import Data.List(foldl')

class ToError e where
    toErr :: e -> Error

data Located a = Located Span a
                 deriving (Show)

data Error = Error [Message]
data Message = Message (Maybe DescriptiveLocation) String

data DescriptiveLocation = At Span
                         | After Span
                         | Before Span
                         deriving (Show)

data Span = Span
            { source :: String
            , start :: Int
            , len :: Int
            , line :: Int
            , col :: Int
            } deriving (Show)

joinSpan :: Span -> Span -> Span
joinSpan a b = a { len = (start b + len b) - start a }

nthLineOf :: String -> Int -> String
nthLineOf src n
    | linesIndex < length lns = lns !! linesIndex
    | otherwise               = ""
    where
        linesIndex = n - 1
        lns = lines src

report :: Error -> IO ()
report (Error messages) =
    putStr finalMessage
    where
        formatMessage :: Message -> String
        formatMessage (Message (Just msgLocation) message) =
             lineNrText ++ nthLineOf src msgLine ++ "\n" ++
             (' ' <$ lineNrText) ++ (' ' <$ [2..msgCol]) ++ underline ++ " " ++ message ++ "\n"
            where
                lineNrText = show msgLine ++ " (:" ++ show msgCol ++ ") | "
                msgSpan = case msgLocation of
                    At s -> s
                    After s -> s
                    Before s -> s

                (Span src _ msgLen msgLine msgCol) = msgSpan

                underline = case msgLocation of
                    At _ -> '^' <$ [1..msgLen]

                    After _ -- easier just to do it manually, probably clearer and easier to understand as well
                        | msgLen == 0 -> ">"
                        | msgLen == 1 -> ">"
                        | msgLen == 2 -> "->"
                        | otherwise   -> "`" ++ ('-' <$ [1..msgLen-2]) ++ ">"

                    Before _ -> "<" ++ ('-' <$ [1..msgLen-1])

        formatMessage (Message Nothing message) = "<somewhere>: " ++ message ++ "\n"

        finalMessage = "error:\n" ++ (foldl' (++) "" $ map formatMessage messages)
