module Diagnostic
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
            , end :: Int
            , line :: Int
            , col :: Int
            } deriving (Show)

joinSpan :: Span -> Span -> Span
joinSpan a b = a { end = end b }

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
             (' ' <$ lineNrText) ++ (' ' <$ [2..msgCol]) ++ (underlineChar <$ [msgStart..msgEnd-1]) ++ " " ++ message ++ "\n"
            where
                lineNrText = show msgLine ++ " (:" ++ show msgCol ++ ") | "
                (underlineChar, msgSpan) = case msgLocation of
                    At s -> ('^', s)
                    After s -> ('>', s)
                    Before s -> ('<', s)
                (Span src msgStart msgEnd msgLine msgCol) = msgSpan

        formatMessage (Message Nothing message) = "<somewhere>: " ++ message ++ "\n"

        finalMessage = "error:\n" ++ (foldl' (++) "" $ map formatMessage messages)
