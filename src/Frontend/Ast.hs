module Frontend.Ast where

import Frontend.Diagnostic

data BinaryOp = Add
              | Sub
              | Mult
              | Div
              | NotEqual
              | Equal
              | Greater
              | GreaterEqual
              | Less
              | LessEqual
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

data Stmt = PrintStmt (Located Expr)
          | ExprStmt (Located Expr)
          deriving Show
