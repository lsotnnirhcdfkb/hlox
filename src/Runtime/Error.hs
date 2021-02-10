module Runtime.Error (RuntimeError(..)) where

import Frontend.Diagnostic

import Runtime.Value

data RuntimeError = OperandsMustBeNumbersOrStrings Span (Located LoxValue) (Located LoxValue)
                  | OperandsMustBeNumbers Span (Located LoxValue) (Located LoxValue)
                  | OperandMustBeNumber Span (Located LoxValue)
                  | UndefinedVariable (Located String)
                  deriving Show

instance LoxError RuntimeError where
    toErr (OperandsMustBeNumbersOrStrings operatorSpan (Located _ lhsValue) (Located _ rhsValue)) = Error
        [ Message (Just $ At operatorSpan) $ "Operands must be two numbers or two strings (" ++ stringifyType lhsValue ++ " and " ++ stringifyType rhsValue ++ " invalid)."
        ]
    toErr (OperandsMustBeNumbers operatorSpan (Located _ lhsValue) (Located _ rhsValue)) = Error
        [ Message (Just $ At operatorSpan) $ "Operands must be numbers (" ++ stringifyType lhsValue ++ " and " ++ stringifyType rhsValue ++ " invalid)."
        ]
    toErr (OperandMustBeNumber operatorSpan (Located _ operandValue)) = Error
        [ Message (Just $ At operatorSpan) $ "Operand must be a number (" ++ stringifyType operandValue ++ " invalid)."
        ]
    toErr (UndefinedVariable (Located span name)) = Error
        [ Message (Just $ At span) $ "Undefined variable '" ++ name ++ "'."
        ]
