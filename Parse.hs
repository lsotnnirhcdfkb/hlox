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
    let Located { value = firstToken } = locatedFirst
    in case firstToken of
        BoolLiteral _   -> parseBoolExpr locatedFirst tokens
        NumberLiteral _ -> parseNumberExpr locatedFirst tokens
        StringLiteral _ -> parseStringExpr locatedFirst tokens
        OpenParen       -> parseGroupingExpr locatedFirst tokens
        Minus           -> parseUnaryExpr locatedFirst tokens
        Bang            -> parseUnaryExpr locatedFirst tokens
        _               -> (Nothing, [ExpectedExpr])

parseBoolExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseBoolExpr locatedToken _ =
    let boolToken = value locatedToken
    in case boolToken of
        BoolLiteral bool -> (Just $ copyLocation (BoolExpr $ copyLocation bool locatedToken) locatedToken, [])
        _ -> error "parse a bool expr where the first token is not a BoolLiteral"

parseNumberExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseNumberExpr locatedToken _ =
    let numToken = value locatedToken
    in case numToken of
        NumberLiteral num -> (Just $ copyLocation (NumberExpr $ copyLocation num locatedToken) locatedToken, [])
        _ -> error "parse a num expr where the first token is not a NumberLiteral"

parseStringExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseStringExpr locatedToken _ =
    let strToken = value locatedToken
    in case strToken of
        StringLiteral str -> (Just $ copyLocation (StringExpr $ copyLocation str locatedToken) locatedToken, [])
        _ -> error "parse a str expr where the first token is not a StringLiteral"

parseGroupingExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseGroupingExpr _ nextTokens =
    let (maybeExpr, errs) = parseExpr nextTokens
        grouped = (\expr -> copyLocation (GroupingExpr expr) expr) <$> maybeExpr
    in (grouped, errs)

parseUnaryExpr :: Located Token -> [Located Token] -> (Maybe (Located Expr), [ParseError])
parseUnaryExpr locOp nextTokens =
    let opTok = value locOp
        operator = case opTok of
            Bang -> Not
            Minus -> Neg
            _ -> error $ "parse a unary expr with invalid unary operator: " ++ show opTok
        (operand, operandErrors) = parseExpr nextTokens
        unaryExpr = (\unexpr -> unionLocation locOp unexpr (UnaryExpr (copyLocation operator locOp) unexpr)) <$> operand
    in (unaryExpr, operandErrors)

