module Diagnostic
    ( Located(..)
    , Span(..)
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
data Message = Message (Maybe Span) String

data Span = Span
    { source :: String
    , start :: Int
    , end :: Int
    , line :: Int
    , col :: Int
    } deriving (Show)

joinSpan :: Span -> Span -> Span
joinSpan a b = a { end = end b }

nthLineOf :: String -> Int -> String
nthLineOf src n
    | linesIndex < length lns = lns !! linesIndex
    | otherwise                            = ""
    where
        linesIndex = n - 1
        lns = lines src

report :: Error -> IO ()
report (Error messages) =
    putStr finalMessage
    where
        formatMessage :: Message -> String
        formatMessage (Message (Just (Span src _ _ msgLine msgCol)) message) =
             lineNrText ++ nthLineOf src msgLine ++ "\n" ++
             (' ' <$ lineNrText) ++ (' ' <$ [2..msgCol]) ++ "^ " ++ message ++ "\n"
            where
                lineNrText = show msgLine ++ " (:" ++ show msgCol ++ ") | "
        formatMessage (Message Nothing message) = "<somewhere>: " ++ message ++ "\n"

        finalMessage = "error:\n" ++ (foldl' (++) "" $ map formatMessage messages)
