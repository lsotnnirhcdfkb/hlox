module Parse (parse) where

import Scan
import Ast
import Diagnostic

-- note: this parser code is kind of janky

data ParseError = ExpectedAfter Span String
                | ExpectedAt Span String
                | ExpectedCloseParen Span Span
    deriving (Show)

instance ToError ParseError where
    toErr (ExpectedAt span expect) = Error
        [ Message (Just $ At span) $ "expected " ++ expect
        ]
    toErr (ExpectedAfter span expect) = Error
        [ Message (Just $ After span) $ "expected " ++ expect
        ]
    toErr (ExpectedCloseParen needParenSpan toMatch) = Error
        [ Message (Just $ After needParenSpan) "expected ')'"
        , Message (Just $ At toMatch) "to match this"
        ]

data Parser = Parser
    { tokens :: [Located Token]
    } deriving (Show)

parse :: [Located Token] -> (Maybe (Located Expr), [ParseError])
parse tokens = (outputAST, errors)
    where
        parser = Parser tokens
        (outputAST, errors, _) = parseExpr parser

type ParserOutput a = (Maybe a, [ParseError], Parser)

advance :: Parser -> Int -> Parser
advance (Parser tokens) n = Parser $ drop n tokens

peek :: Parser -> Located Token
peek (Parser { tokens = (firstToken:_) }) = firstToken
peek (Parser { tokens = [] }) = error "peek empty parser"

parseExpr :: Parser -> ParserOutput (Located Expr)
parseExpr (Parser []) = error "parser should never be empty: there should be an eof token that terminates it"
parseExpr parser =
    let locatedFirstToken@(Located firstTokenSpan firstToken):_ = tokens parser
        newparser = parser `advance` 1
    in case firstToken of
        BoolLiteral _   -> newparser `parseBoolExpr` locatedFirstToken
        NumberLiteral _ -> newparser `parseNumberExpr` locatedFirstToken
        StringLiteral _ -> newparser `parseStringExpr` locatedFirstToken
        OpenParen       -> newparser `parseGroupingExpr` locatedFirstToken
        Minus           -> newparser `parseUnaryExpr` locatedFirstToken
        Bang            -> newparser `parseUnaryExpr` locatedFirstToken
        _               -> (Nothing, [ExpectedAt firstTokenSpan "expression"], parser)

parseBoolExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseBoolExpr parser (Located boolSpan boolToken) =
    case boolToken of
        BoolLiteral bool -> (Just $ Located boolSpan $ BoolExpr $ Located boolSpan bool, [], parser)
        _ -> error "parse a bool expr where the first token is not a BoolLiteral"

parseNumberExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseNumberExpr parser (Located numberSpan numberToken) =
    case numberToken of
        NumberLiteral num -> (Just $ Located numberSpan $ NumberExpr $ Located numberSpan num, [], parser)
        _ -> error "parse a num expr where the first token is not a NumberLiteral"

parseStringExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseStringExpr parser (Located stringSpan stringToken) =
    case stringToken of
        StringLiteral str -> (Just $ Located stringSpan $ StringExpr $ Located stringSpan str, [], parser)
        _ -> error "parse a str expr where the first token is not a StringLiteral"

parseGroupingExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseGroupingExpr parser (Located openParenSpan _) =
    let (maybeExpr, errs, parser') = parseExpr parser
        (groupedExpr, parenErrs, parser'') = case maybeExpr of
            Just locatedInsideParenExpr@(Located insideParenSpan _) ->
                let (maybeCloseParenSpan, parser'', parenErrs) = case peek parser' of
                        Located closeParenSpan CloseParen -> (Just closeParenSpan, parser' `advance` 1, [])
                        _ -> (Nothing, parser', [ExpectedCloseParen insideParenSpan openParenSpan])
                    groupedExpr = (\closeParenSpan ->
                        Located (openParenSpan `joinSpan` closeParenSpan) (GroupingExpr locatedInsideParenExpr)) <$> maybeCloseParenSpan
                in (groupedExpr, parenErrs, parser'')

            Nothing -> (Nothing, [], parser')

    in (groupedExpr, errs ++ parenErrs, parser'')

parseUnaryExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseUnaryExpr parser (Located operatorSpan operatorToken) =
    let operator = Located operatorSpan $ case operatorToken of
            Bang -> Not
            Minus -> Neg
            _ -> error $ "parse a unary expr with invalid unary operator: " ++ show operatorToken
        (maybeOperand, operandErrors, nextParser) = parseExpr parser
        unaryExpr = (\locatedOperand@(Located operandSpan _) -> Located (operatorSpan `joinSpan` operandSpan) $ UnaryExpr operator locatedOperand) <$> maybeOperand
    in (unaryExpr, operandErrors, nextParser)

