module Treewalk.Interpret (interpret) where

import Frontend.Ast
import Frontend.Diagnostic
import Runtime.Value

interpret :: Located Expr -> LoxValue
interpret (Located _ (BinaryExpr lhs (Located _ operator) rhs)) =
    let lhsEval = interpret lhs
        rhsEval = interpret rhs
        lhsDouble = case lhsEval of
            LoxNumber d -> d
            _ -> error "invalid cast"
        rhsDouble = case rhsEval of
            LoxNumber d -> d
            _ -> error "invalid cast"

    in case operator of
        Add  -> LoxNumber $ lhsDouble + rhsDouble
        Sub  -> LoxNumber $ lhsDouble - rhsDouble
        Mult -> LoxNumber $ lhsDouble * rhsDouble
        Div  -> LoxNumber $ lhsDouble / rhsDouble

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
                _ -> error "invalid cast"

        Not -> LoxBool $ not (isTruthy operandValue)

interpret (Located _ (GroupingExpr expr)) = interpret expr
interpret (Located _ (BoolExpr (Located _ b))) = LoxBool b
interpret (Located _ (NumberExpr (Located _ n))) = LoxNumber n
interpret (Located _ (StringExpr (Located _ s))) = LoxString s

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True
