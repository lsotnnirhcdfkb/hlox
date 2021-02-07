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

data Error = Error (Maybe Span) String [Message]
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

report :: Error -> IO ()
report (Error mainLocation mainMessage messages) =
    putStr finalMessage
    where
        putLocation :: Maybe Span -> String
        putLocation (Just (Span source start end line col)) =
            let substr = (take $ end - start) . drop start $ source
            in "[line " ++ show line ++ ", col " ++ show col ++ ", at '" ++ substr ++ "']"
        putLocation Nothing = "somewhere"

        formatMessage :: Message -> String
        formatMessage (Message location message) = " - " ++ putLocation location ++ ": " ++ message ++ "\n"

        heading = "Error at " ++ putLocation mainLocation ++ ": " ++ mainMessage ++ "\n"

        finalMessage = heading ++ (foldl' (++) "" $ map formatMessage messages)
