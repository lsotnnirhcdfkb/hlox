module Parse (parse) where

import Scan
import Ast
import Diagnostic

data ParseError = UnexpectedEOF
                | ExpectedExpr
    deriving (Show)

parse :: [Located Token] -> (Maybe (Located Expr), [ParseError])
parse = parseExpr

parseExpr :: [Located Token] -> (Maybe (Located Expr), [ParseError])
parseExpr [] = (Nothing, [UnexpectedEOF])
parseExpr (locatedFirst:tokens) =
    let Located _ firstToken = locatedFirst
    in case firstToken of
        BoolLiteral _   -> parseBoolExpr locatedFirst tokens
        NumberLiteral _ -> parseNumberExpr locatedFirst tokens
        StringLiteral _ -> parseStringExpr locatedFirst tokens
        OpenParen       -> parseGroupingExpr locatedFirst tokens
        Minus           -> parseUnaryExpr locatedFirst tokens
        Bang            -> parseUnaryExpr locatedFirst tokens
        _               -> (Nothing, [ExpectedExpr])

parseBoolExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseBoolExpr (Located boolSpan boolToken) _ =
    case boolToken of
        BoolLiteral bool -> (Just $ Located boolSpan $ BoolExpr $ Located boolSpan bool, [])
        _ -> error "parse a bool expr where the first token is not a BoolLiteral"

parseNumberExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseNumberExpr (Located numberSpan numberToken) _ =
    case numberToken of
        NumberLiteral num -> (Just $ Located numberSpan $ NumberExpr $ Located numberSpan num, [])
        _ -> error "parse a num expr where the first token is not a NumberLiteral"

parseStringExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseStringExpr (Located stringSpan stringToken) _ =
    case stringToken of
        StringLiteral str -> (Just $ Located stringSpan $ StringExpr $ Located stringSpan str, [])
        _ -> error "parse a str expr where the first token is not a StringLiteral"

parseGroupingExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseGroupingExpr (Located _ _) nextTokens =
    let (maybeExpr, errs) = parseExpr nextTokens
        grouped = (\groupedAndSpan@(Located exprSpan _) -> Located exprSpan $ GroupingExpr groupedAndSpan) <$> maybeExpr
    in (grouped, errs)

parseUnaryExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseUnaryExpr (Located operatorSpan operatorToken) nextTokens =
    let operator = Located operatorSpan $ case operatorToken of
            Bang -> Not
            Minus -> Neg
            _ -> error $ "parse a unary expr with invalid unary operator: " ++ show operatorToken
        (maybeOperand, operandErrors) = parseExpr nextTokens
        unaryExpr = (\locatedOperand@(Located operandSpan _) -> Located (joinSpan operatorSpan operandSpan) $ UnaryExpr operator locatedOperand) <$> maybeOperand
    in (unaryExpr, operandErrors)

