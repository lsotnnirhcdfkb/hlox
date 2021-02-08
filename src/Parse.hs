module Parse (parse) where

import Scan
import Ast
import Diagnostic

-- note: this parser code is kind of janky

data ParseError = Expected DescriptiveLocation String (Maybe String)
                | ExpectedCloseParen Span Span
                | UnaryPlusUnsupported Span
                deriving (Show)

instance ToError ParseError where
    toErr (Expected location expect representing) = Error
        [ Message (Just location) $ "expected " ++ expect ++ representingStr
        ]
        where representingStr = case representing of
                Just r -> " as " ++ r
                Nothing -> ""

    toErr (ExpectedCloseParen needParenSpan toMatch) = Error
        [ Message (Just $ After needParenSpan) "expected ')'"
        , Message (Just $ At toMatch) "to match this"
        ]

    toErr (UnaryPlusUnsupported plusSpan) = Error
        [ Message (Just $ At plusSpan) "unary + is not supported"
        ]

data Parser = Parser
    { tokens :: [Located Token]
    } deriving (Show)

parse :: [Located Token] -> (Maybe (Located Expr), [ParseError])
parse tokens = (outputAST, errors)
    where
        parser = Parser tokens
        (outputAST, errors, _) = parseExpr parser 0 ""

type ParserOutput a = (Maybe a, [ParseError], Parser)

advance :: Parser -> Int -> Parser
advance (Parser tokens) n = Parser $ drop n tokens

peek :: Parser -> Located Token
peek (Parser { tokens = (firstToken:_) }) = firstToken
peek (Parser { tokens = [] }) = error "peek empty parser"

parseExpr :: Parser -> Int -> String -> ParserOutput (Located Expr)
parseExpr (Parser []) _ _ = error "parser should never be empty! there should be an eof token that terminates it"
parseExpr parser prec representing =
    let (prefixParsedExpr, prefixErrors, parser') = prefixParse parser representing
        (infixParsedExpr, infixErrors, parser'') = case prefixParsedExpr of
            Just e -> infixParse parser' e prec
            Nothing -> (Nothing, [], parser')
    in (infixParsedExpr, prefixErrors ++ infixErrors, parser'')

prefixParse :: Parser -> String -> ParserOutput (Located Expr)
prefixParse parser representing =
    let locatedFirstToken@(Located firstTokenSpan firstToken):_ = tokens parser
        chosenPrefixParseFunc = case firstToken of
            BoolLiteral _   -> Just parseTokenExpr
            NumberLiteral _ -> Just parseTokenExpr
            StringLiteral _ -> Just parseTokenExpr
            OpenParen       -> Just parseGroupingExpr
            Minus           -> Just parseUnaryExpr
            Bang            -> Just parseUnaryExpr
            Plus            -> Just parseUnaryPositiveExpr
            _               -> Nothing
    in case chosenPrefixParseFunc of
        Just func -> (parser `advance` 1) `func` locatedFirstToken
        Nothing   -> (Nothing, [Expected (At firstTokenSpan) "expression" $ Just representing], parser)

getFirstToken :: Parser -> Token
getFirstToken (Parser ((Located _ tok):_)) = tok

precedenceOf :: Token -> Int
precedenceOf Minus = 5
precedenceOf Plus = 5
precedenceOf Star = 6
precedenceOf Slash = 6
precedenceOf Bang = 7
precedenceOf Minus = 7
precedenceOf _ = 0

infixParse :: Parser -> Located Expr -> Int -> ParserOutput (Located Expr)
infixParse parser lhs prec
    | (precedenceOf $ getFirstToken parser) > prec =
        let locatedOperatorToken@(Located operatorSpan operatorToken):_ = tokens parser
            chosenParseFunc = case operatorToken of
                Plus        -> Just parseBinaryExpr
                Minus       -> Just parseBinaryExpr
                Star        -> Just parseBinaryExpr
                Slash       -> Just parseBinaryExpr
                _           -> Nothing
        in case chosenParseFunc of
                Just func ->
                    let (maybeInfixParsed, infixErrors, afterInfixParser) = func (parser `advance` 1) lhs locatedOperatorToken
                    in case maybeInfixParsed of
                        Just newLhs -> infixParse afterInfixParser newLhs prec
                        Nothing -> (Nothing, infixErrors, afterInfixParser)
                Nothing   -> (Just lhs, [], parser)
    | otherwise = (Just lhs, [], parser)

parseBinaryExpr :: Parser -> Located Expr -> Located Token -> ParserOutput (Located Expr)
parseBinaryExpr parser locatedLhs@(Located lhsSpan lhs) locatedOperator@(Located operatorSpan operatorToken) =
    let (binaryOperator', rhsOf) = case operatorToken of
            Plus  -> (Add, "addition")
            Minus -> (Sub, "subtraction")
            Star  -> (Mult, "multiplication")
            Slash -> (Div, "division")
            _     -> error "invalid binary operator"
        binaryOperator = Located operatorSpan binaryOperator'
        (maybeRhs, rhsErrors, rhsParser) = parseExpr parser (precedenceOf operatorToken + 1) $ "right hand side to " ++ rhsOf ++ " expression"
        maybeBinary = (\locatedRhs@(Located rhsSpan rhs) -> Located (lhsSpan `joinSpan` rhsSpan) $ BinaryExpr locatedLhs binaryOperator locatedRhs) <$> maybeRhs
    in (maybeBinary, rhsErrors, rhsParser)

parseTokenExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseTokenExpr parser (Located tokenSpan token) =
    let expr = case token of
            BoolLiteral bool  -> BoolExpr $ Located tokenSpan bool
            NumberLiteral num -> NumberExpr $ Located tokenSpan num
            StringLiteral str -> StringExpr $ Located tokenSpan str
            _ -> error "parse a bool expr where the first token is not a BoolLiteral"
    in (Just $ Located tokenSpan expr, [], parser)

parseGroupingExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseGroupingExpr parser (Located openParenSpan _) =
    let (maybeExpr, errs, parser') = parseExpr parser 0 "grouped expression inside parentheses"
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
    let (operator', unaryExprType) = case operatorToken of
            Bang -> (Not, "logical not")
            Minus -> (Neg, "negation")
            _ -> error $ "parse a unary expr with invalid unary operator: " ++ show operatorToken
        operator = Located operatorSpan operator'
        (maybeOperand, operandErrors, nextParser) = parseExpr parser (precedenceOf operatorToken) $ "operand to " ++ unaryExprType ++ " expression"
        unaryExpr = (\locatedOperand@(Located operandSpan _) -> Located (operatorSpan `joinSpan` operandSpan) $ UnaryExpr operator locatedOperand) <$> maybeOperand
    in (unaryExpr, operandErrors, nextParser)

parseUnaryPositiveExpr :: Parser -> Located Token -> ParserOutput (Located Expr)
parseUnaryPositiveExpr parser (Located operatorSpan operatorToken) =
    let (_, operandErrors, nextParser) = parseExpr parser (precedenceOf Minus) $ "operand to unary + expression"
    in (Nothing, (UnaryPlusUnsupported operatorSpan):operandErrors, nextParser)
