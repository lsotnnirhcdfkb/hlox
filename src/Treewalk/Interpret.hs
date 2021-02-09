module Treewalk.Interpret (interpret) where

import Frontend.Ast
import Frontend.Diagnostic
import Runtime.Value
import Runtime.Error

interpret :: Located Expr -> Either RuntimeError (Located LoxValue)
interpret (Located outSpan (BinaryExpr lhs@(Located lhsSpan _) (Located operatorSpan operator) rhs@(Located rhsSpan _))) =
    interpret lhs >>= \locatedLhsValue@(Located _ lhsValue) ->
    interpret rhs >>= \locatedRhsValue@(Located _ rhsValue) ->
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

interpret (Located outSpan (UnaryExpr (Located operatorSpan operator) operandAST)) =
    interpret operandAST >>= \loctedOperand@(Located _ operandValue) ->
    case operator of
        Neg ->
            case operandValue of
                LoxNumber d -> Right $ Located outSpan $ LoxNumber $ -d
                _ -> error "Operand must be a number"

        Not -> Right $ Located outSpan $ LoxBool $ not (isTruthy operandValue)

interpret (Located groupingSpan (GroupingExpr expr)) =
    interpret expr >>= \(Located _ insideValue) ->
    Right $ Located groupingSpan $ insideValue

interpret (Located span (BoolExpr (Located _ b))) = Right $ Located span $ LoxBool b
interpret (Located span (NumberExpr (Located _ n))) = Right $ Located span $ LoxNumber n
interpret (Located span (StringExpr (Located _ s))) = Right $ Located span $ LoxString s

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

checkOperandsDouble :: Span -> Located LoxValue -> Located LoxValue -> Either RuntimeError (Double, Double)
checkOperandsDouble _ (Located _ (LoxNumber lhs)) (Located _ (LoxNumber rhs)) = Right (lhs, rhs)
checkOperandsDouble operatorSpan lhs rhs = Left $ OperandsMustBeNumbers operatorSpan lhs rhs
