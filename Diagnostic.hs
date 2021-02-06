module Diagnostic (Located(..), copyLocation, unionLocation) where

data Located a =
    Located {
        value :: a,
        start :: Int,
        end :: Int,
        line :: Int,
        col :: Int
    }
    deriving (Show)

copyLocation :: b -> Located a -> Located b
copyLocation val loc = Located {
        value = val,
        start = start loc,
        end = end loc,
        line = line loc,
        col = col loc
    }

unionLocation :: Located a -> Located b -> c -> Located c
unionLocation a b val = Located {
        value = val,
        start = start a,
        end = end b,
        line = line a,
        col = col a
    }
