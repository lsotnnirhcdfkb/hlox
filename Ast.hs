module Ast (Expr(..), BinaryOp(..), UnaryOp(..)) where

import Diagnostic

data BinaryOp = Add
              | Sub
              | Mult
              | Div

data UnaryOp = Neg
             | Not

data Expr = BinaryExpr (Located Expr) (Located BinaryOp) (Located Expr)
          | GroupingExpr (Located Expr)
          | BoolExpr (Located Bool)
          | NumberExpr (Located Double)
          | StringExpr (Located String)
          | UnaryExpr (Located UnaryOp) (Located Expr)
