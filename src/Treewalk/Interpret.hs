module Treewalk.Interpret
    ( interpretStmt
    , interpretExpr
    , interpretScript
    ) where

import qualified Data.Map as Map

import Frontend.Ast
import Frontend.Diagnostic
import Runtime.Value
import Runtime.Error
import Treewalk.State

interpretScript :: [Located Stmt] -> IO (Either RuntimeError ())
interpretScript stmts =
    interpretStmts baseState stmts >>= \ei ->
    return $ ei >> Right ()
    where
        baseState = InterpreterState {}

interpretStmts :: InterpreterState -> [Located Stmt] -> IO (Either (RuntimeError, InterpreterState) InterpreterState)
interpretStmts state (stmt:stmts) =
    interpretStmt state stmt >>= \stmtRes ->
    case stmtRes of
        Left re -> return $ Left re
        Right state' ->
            interpretStmts state' stmts
interpretStmts state [] = return $ Right state

interpretStmt :: InterpreterState -> Located Stmt -> IO (Either RuntimeError InterpreterState)
interpretStmt state (Located _ (PrintStmt expr)) =
    case interpretExpr expr of
        Left re -> return $ Left re
        Right (Located _ value) ->
            putStrLn (stringifyValue value) >>
            return (Right state)

interpretStmt state (Located _ (ExprStmt expr)) =
    return $ interpretExpr expr >> Right state

interpretStmt state (Located _ (VarStmt (Located nameSpan varName) maybeInitializer)) =
    let eiInit = case maybeInitializer of
            Just init -> interpretExpr init
            Nothing -> Right $ Located nameSpan LoxNil
    in return $ eiInit >>= \(Located _ initializer) ->
    Right $ defineVar state varName initializer

interpretExpr :: Located Expr -> Either RuntimeError (Located LoxValue)
interpretExpr (Located outSpan (BinaryExpr lhs@(Located _ _) (Located operatorSpan operator) rhs@(Located _ _))) =
    interpretExpr lhs >>= \locatedLhsValue@(Located _ lhsValue) ->
    interpretExpr rhs >>= \locatedRhsValue@(Located _ rhsValue) ->
    let checkDoubles = checkOperandsDouble operatorSpan locatedLhsValue locatedRhsValue
    in case operator of
        Sub ->
            checkDoubles >>= \(lhsDouble, rhsDouble) ->
            out $ LoxNumber $ lhsDouble - rhsDouble
        Mult ->
            checkDoubles >>= \(lhsDouble, rhsDouble) ->
            out $ LoxNumber $ lhsDouble * rhsDouble
        Div ->
            checkDoubles >>= \(lhsDouble, rhsDouble) ->
            out $ LoxNumber $ lhsDouble / rhsDouble
        Add ->
            case (lhsValue, rhsValue) of
                (LoxNumber lhsDouble, LoxNumber rhsDouble) -> out $ LoxNumber $ lhsDouble + rhsDouble
                (LoxString lhsStr, LoxString rhsStr) -> out $ LoxString $ lhsStr ++ rhsStr

                _ -> Left $ OperandsMustBeNumbersOrStrings operatorSpan locatedLhsValue locatedRhsValue

        Greater      ->
            checkDoubles >>= \(lhsDouble, rhsDouble) ->
            out $ LoxBool $ lhsDouble > rhsDouble
        GreaterEqual ->
            checkDoubles >>= \(lhsDouble, rhsDouble) ->
            out $ LoxBool $ lhsDouble >= rhsDouble
        Less         ->
            checkDoubles >>= \(lhsDouble, rhsDouble) ->
            out $ LoxBool $ lhsDouble < rhsDouble
        LessEqual    ->
            checkDoubles >>= \(lhsDouble, rhsDouble) ->
            out $ LoxBool $ lhsDouble <= rhsDouble

        Equal    -> out $ LoxBool $ lhsValue == rhsValue
        NotEqual -> out $ LoxBool $ not $ lhsValue == rhsValue

    where
        out value = Right $ Located outSpan value

interpretExpr (Located outSpan (UnaryExpr (Located operatorSpan operator) operandAST)) =
    interpretExpr operandAST >>= \locatedOperand@(Located _ operandValue) ->
    case operator of
        Neg ->
            case operandValue of
                LoxNumber d -> Right $ Located outSpan $ LoxNumber $ -d
                _ -> Left $ OperandMustBeNumber operatorSpan locatedOperand

        Not -> Right $ Located outSpan $ LoxBool $ not (isTruthy operandValue)

interpretExpr (Located groupingSpan (GroupingExpr expr)) =
    interpretExpr expr >>= \(Located _ insideValue) ->
    Right $ Located groupingSpan $ insideValue

interpretExpr (Located span (BoolExpr b)) = Right $ Located span $ LoxBool b
interpretExpr (Located span (NumberExpr n)) = Right $ Located span $ LoxNumber n
interpretExpr (Located span (StringExpr s)) = Right $ Located span $ LoxString s
interpretExpr (Located span NilExpr) = Right $ Located span LoxNil
interpretExpr (Located span (VarExpr name)) = Left $ UndefinedVariable $ Located span name

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

checkOperandsDouble :: Span -> Located LoxValue -> Located LoxValue -> Either RuntimeError (Double, Double)
checkOperandsDouble _ (Located _ (LoxNumber lhs)) (Located _ (LoxNumber rhs)) = Right (lhs, rhs)
checkOperandsDouble operatorSpan lhs rhs = Left $ OperandsMustBeNumbers operatorSpan lhs rhs
