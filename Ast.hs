module Ast (Expr(..), BinaryOp(..), UnaryOp(..)) where

import Diagnostic

data BinaryOp = Add
              | Sub
              | Mult
              | Div
              deriving Show

data UnaryOp = Neg
             | Not
             deriving Show

data Expr = BinaryExpr (Located Expr) (Located BinaryOp) (Located Expr)
          | GroupingExpr (Located Expr)
          | BoolExpr (Located Bool)
          | NumberExpr (Located Double)
          | StringExpr (Located String)
          | UnaryExpr (Located UnaryOp) (Located Expr)
          deriving Show
