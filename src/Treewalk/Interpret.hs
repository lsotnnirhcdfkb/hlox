module Treewalk.Interpret (interpret) where

import Frontend.Ast
import Frontend.Diagnostic
import Runtime.Value

interpret :: Located Expr -> LoxValue
interpret (Located _ (BinaryExpr lhs (Located operatorSpan operator) rhs)) =
    let lhsEval = interpret lhs
        rhsEval = interpret rhs
        (lhsDouble, rhsDouble) = checkOperandsDouble operatorSpan lhsEval rhsEval

    in case operator of
        Sub  -> LoxNumber $ lhsDouble - rhsDouble
        Mult -> LoxNumber $ lhsDouble * rhsDouble
        Div  -> LoxNumber $ lhsDouble / rhsDouble

        Add -> case (lhsEval, rhsEval) of
            (LoxNumber lhsDouble, LoxNumber rhsDouble) -> LoxNumber $ lhsDouble + rhsDouble
            (LoxString lhsStr, LoxString rhsStr) -> LoxString $ lhsStr ++ rhsStr
            _ -> error "Operands must be two numbers or two strings"

        Greater      -> LoxBool $ lhsDouble > rhsDouble
        GreaterEqual -> LoxBool $ lhsDouble >= rhsDouble
        Less         -> LoxBool $ lhsDouble < rhsDouble
        LessEqual    -> LoxBool $ lhsDouble <= rhsDouble

        Equal    -> LoxBool $ lhsEval == rhsEval
        NotEqual -> LoxBool $ not $ lhsEval == rhsEval

interpret (Located _ (UnaryExpr (Located _ operator) operand)) = 
    let operandValue = interpret operand
    in case operator of
        Neg ->
            case operandValue of
                LoxNumber d -> LoxNumber $ -d
                _ -> error "Operand must be a number"

        Not -> LoxBool $ not (isTruthy operandValue)

interpret (Located _ (GroupingExpr expr)) = interpret expr
interpret (Located _ (BoolExpr (Located _ b))) = LoxBool b
interpret (Located _ (NumberExpr (Located _ n))) = LoxNumber n
interpret (Located _ (StringExpr (Located _ s))) = LoxString s

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

checkOperandsDouble :: Span -> LoxValue -> LoxValue -> (Double, Double)
checkOperandsDouble _ (LoxNumber lhs) (LoxNumber rhs) = (lhs, rhs)
checkOperandsDouble _ _ _ = error "Operands must be numbers"
