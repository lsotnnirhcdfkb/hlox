module Runtime.Error (RuntimeError(..)) where

import Frontend.Diagnostic

import Runtime.Value

data RuntimeError = OperandsMustBeNumbersOrStrings Span (Located LoxValue) (Located LoxValue)
                  | OperandsMustBeNumbers Span (Located LoxValue) (Located LoxValue)
                  | OperandMustBeNumber Span (Located LoxValue)
                  deriving Show

instance LoxError RuntimeError where
    toErr (OperandsMustBeNumbersOrStrings operatorSpan (Located lhsSpan lhsValue) (Located rhsSpan rhsValue)) = Error
        [ Message (Just $ At operatorSpan) $ "Operands must be two numbers or two strings (" ++ stringifyType lhsValue ++ " and " ++ stringifyType rhsValue ++ " invalid)."
        ]
    toErr (OperandsMustBeNumbers operatorSpan (Located lhsSpan lhsValue) (Located rhsSpan rhsValue)) = Error
        [ Message (Just $ At operatorSpan) $ "Operands must be numbers (" ++ stringifyType lhsValue ++ " and " ++ stringifyType rhsValue ++ " invalid)."
        ]
    toErr (OperandMustBeNumber operatorSpan (Located operandSpan operandValue)) = Error
        [ Message (Just $ At operatorSpan) $ "Operand must be a number (" ++ stringifyType operandValue ++ " invalid)."
        ]
