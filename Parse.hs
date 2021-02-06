module Parse (parse) where

import Scan
import Ast
import Diagnostic

parse :: [Located Token] -> Located Expr
parse = parseExpr

parseExpr :: [Located Token] -> Located Expr
parseExpr [] = error "unexpected eof while parsing (this error should eventually be replaced Left ParseError)"
parseExpr tokens =
    let (Located { value = firstToken }:_) = tokens
    in case firstToken of
        BoolLiteral _ -> parseBoolExpr tokens
        NumberLiteral _ -> parseNumberExpr tokens
        StringLiteral _ -> parseStringExpr tokens
        OpenParen -> parseGroupingExpr tokens
        Minus -> parseUnaryExpr tokens
        Bang -> parseUnaryExpr tokens
        _ -> error "expected expr"

parseBoolExpr :: [Located Token] -> Located Expr
parseBoolExpr [] = error "parse a bool expr with empty token stream that was not filtered out by parseExpr"
parseBoolExpr (locatedToken:_) =
    let boolToken = value locatedToken
    in case boolToken of
        BoolLiteral bool -> copyLocation (BoolExpr $ copyLocation bool locatedToken) locatedToken
        _ -> error "parse a bool expr where the first token is not a BoolLiteral"

parseNumberExpr :: [Located Token] -> Located Expr
parseNumberExpr [] = error "parse a number expr with empty token stream that was not filtered out by parseExpr"
parseNumberExpr (locatedToken:_) =
    let numToken = value locatedToken
    in case numToken of
        NumberLiteral num -> copyLocation (NumberExpr $ copyLocation num locatedToken) locatedToken
        _ -> error "parse a num expr where the first token is not a NumberLiteral"

parseStringExpr :: [Located Token] -> Located Expr
parseStringExpr [] = error "parse a string expr with empty token stream that was not filtered out by parseExpr"
parseStringExpr (locatedToken:_) =
    let strToken = value locatedToken
    in case strToken of
        StringLiteral str -> copyLocation (StringExpr $ copyLocation str locatedToken) locatedToken
        _ -> error "parse a str expr where the first token is not a StringLiteral"

parseGroupingExpr :: [Located Token] -> Located Expr
parseGroupingExpr [] = error "parse a grouping expr with empty token stream that was not filtered out by parseExpr"
parseGroupingExpr (_:nextTokens) =
    let expr = parseExpr nextTokens
    in copyLocation (GroupingExpr expr) expr

parseUnaryExpr :: [Located Token] -> Located Expr
parseUnaryExpr [] = error "parse a unary expr with empty token stream that was not filtered out by parseExpr"
parseUnaryExpr (locOp:nextTokens) =
    let opTok = value locOp
        operator = case opTok of
            Bang -> Not
            Minus -> Neg
            _ -> error $ "parse a unary expr with invalid unary operator: " ++ show opTok
        operand = parseExpr nextTokens
    in unionLocation locOp operand $ UnaryExpr (copyLocation operator locOp) operand

