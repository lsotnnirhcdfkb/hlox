module Diagnostic (Located(..), Span(..), joinSpan) where

data Located a = Located Span a
    deriving (Show)

data Span = Span
    { start :: Int
    , end :: Int
    , line :: Int
    , col :: Int
    } deriving (Show)

joinSpan :: Span -> Span -> Span
joinSpan a b = a { end = end b }
