module Runtime.Error (RuntimeError(..)) where

import Frontend.Diagnostic

import Runtime.Value

data RuntimeError = OperandsMustBeNumbersOrStrings Span (Located LoxValue) (Located LoxValue)
                  | OperandsMustBeNumbers Span (Located LoxValue) (Located LoxValue)
                  | OperandMustBeNumber Span (Located LoxValue)
                  deriving Show

instance LoxError RuntimeError where
    toErr (OperandsMustBeNumbersOrStrings operatorSpan (Located lhsSpan lhsValue) (Located rhsSpan rhsValue)) = Error
        [ Message (Just $ At operatorSpan) "Operands must be two numbers or two strings."
        , Message (Just $ At lhsSpan) $ "a " ++ stringifyType lhsValue
        , Message (Just $ At rhsSpan) $ "a " ++ stringifyType rhsValue
        ]
    toErr (OperandsMustBeNumbers operatorSpan (Located lhsSpan lhsValue) (Located rhsSpan rhsValue)) = Error
        [ Message (Just $ At operatorSpan) "Operands must be numbers."
        , Message (Just $ At lhsSpan) $ "a " ++ stringifyType lhsValue
        , Message (Just $ At rhsSpan) $ "a " ++ stringifyType rhsValue
        ]
    toErr (OperandMustBeNumber operatorSpan (Located operandSpan operandValue)) = Error
        [ Message (Just $ At operatorSpan) "Operand must be a number."
        , Message (Just $ At operandSpan) $ "a " ++ stringifyType operandValue
        ]
