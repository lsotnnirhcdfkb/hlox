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
          | BoolExpr Bool
          | NumberExpr Double
          | StringExpr String
          | NilExpr
          | UnaryExpr (Located UnaryOp) (Located Expr)
          | VarExpr String
          deriving Show

data Stmt = PrintStmt (Located Expr)
          | ExprStmt (Located Expr)
          | VarStmt (Located String) (Maybe (Located Expr))
          deriving Show
