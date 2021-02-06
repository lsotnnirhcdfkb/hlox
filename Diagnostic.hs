module Diagnostic (Located(..)) where

data Located a =
    Located {
        value :: a,
        start :: Int,
        end :: Int,
        line :: Int,
        col :: Int
    }
    deriving (Show)
