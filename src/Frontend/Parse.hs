module Frontend.Parse (parse) where

import Frontend.Scan
import Frontend.Ast
import Frontend.Diagnostic

-- note: this parser code is kind of janky

data ParseError = Expected DescriptiveLocation String (Maybe String)
                | ExpectedCloseParen Span Span
                | UnaryPlusUnsupported Span
                deriving (Show)

instance LoxError ParseError where
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

parse :: [Located Token] -> ([Located Stmt], [ParseError])
parse tokens = (outputAST, errors)
    where
        parser = Parser tokens
        (outputAST, errors, _) = parseStmts parser

type ParserOutput a = (a, [ParseError], Parser)

advance :: Parser -> Int -> Parser
advance (Parser tokens) n = Parser $ drop n tokens

peek :: Parser -> Located Token
peek (Parser { tokens = (firstToken:_) }) = firstToken
peek (Parser { tokens = [] }) = error "peek empty parser"

getFirstToken :: Parser -> Token
getFirstToken (Parser []) = error "get first token of empty parser, which should never happen"
getFirstToken (Parser ((Located _ tok):_)) = tok

parseStmts :: Parser -> ParserOutput [Located Stmt]
parseStmts parser =
    case getFirstToken parser of
        Eof -> ([], [], parser)
        _ ->
            let (maybeStmt, singleErrs, parser') = parseStmt parser
                (nextStmts, nextErrs, parser'') = parseStmts parser'

                totalStmts = case maybeStmt of
                    Just s -> s:nextStmts
                    Nothing -> nextStmts

                totalErrs = singleErrs ++ nextErrs

            in (totalStmts, totalErrs, parser'')

parseStmt :: Parser -> ParserOutput (Maybe (Located Stmt))
parseStmt parser =
    let first = getFirstToken parser
    in case first of
        Print -> parsePrintStmt parser
        _ -> parseExprStmt parser

-- TODO FIX: consume semicolon
parsePrintStmt :: Parser -> ParserOutput (Maybe (Located Stmt))
parsePrintStmt parser =
    let (Located printSpan _) = peek parser
        (maybeExpr, errs, parser') = parseExpr (parser `advance` 1) 0 "expression to print"

        (printStmt, semiErrs, parser'') = case maybeExpr of
            Just exprToPrint@(Located exprSpan _) ->
                case peek parser' of
                    Located semiSpan Semicolon ->
                        (Just $ Located (printSpan `joinSpan` semiSpan) $ PrintStmt exprToPrint, [], parser' `advance` 1)
                    _ -> (Nothing, [Expected (After exprSpan) "';'" Nothing], parser')
            Nothing -> (Nothing, [], parser')

    in (printStmt, errs ++ semiErrs, parser'')

parseExprStmt :: Parser -> ParserOutput (Maybe (Located Stmt))
parseExprStmt parser =
    let (maybeExpr, errs, parser') = parseExpr parser 0 "expression statement"
    in case maybeExpr of
        Just locatedExpr@(Located exprSpan _) ->
            case peek parser' of
                Located semiSpan Semicolon -> (Just $ Located (exprSpan `joinSpan` semiSpan) $ ExprStmt locatedExpr, errs, parser' `advance` 1)
                _ -> (Nothing, errs ++ [Expected (After exprSpan) "';'" Nothing], parser')
        Nothing -> (Nothing, errs, parser')

parseExpr :: Parser -> Int -> String -> ParserOutput (Maybe (Located Expr))
parseExpr (Parser []) _ _ = error "parser should never be empty! there should be an eof token that terminates it"
parseExpr parser prec representing =
    let (prefixParsedExpr, prefixErrors, parser') = prefixParse parser representing
        (infixParsedExpr, infixErrors, parser'') = case prefixParsedExpr of
            Just e -> infixParse parser' e prec
            Nothing -> (Nothing, [], parser')
    in (infixParsedExpr, prefixErrors ++ infixErrors, parser'')

prefixParse :: Parser -> String -> ParserOutput (Maybe (Located Expr))
prefixParse parser representing =
    let locatedFirstToken@(Located firstTokenSpan firstToken):_ = tokens parser
        chosenPrefixParseFunc = case firstToken of
            BoolLiteral _   -> Just parseTokenExpr
            NumberLiteral _ -> Just parseTokenExpr
            StringLiteral _ -> Just parseTokenExpr
            NilLiteral      -> Just parseTokenExpr
            OpenParen       -> Just parseGroupingExpr
            Minus           -> Just parseUnaryExpr
            Bang            -> Just parseUnaryExpr
            Plus            -> Just parseUnaryPositiveExpr
            _               -> Nothing
    in case chosenPrefixParseFunc of
        Just func -> (parser `advance` 1) `func` locatedFirstToken
        Nothing   -> (Nothing, [Expected (At firstTokenSpan) "expression" $ Just representing], parser)

precedenceAssignment = 1 :: Int
precedenceOr = 2 :: Int
precedenceAnd = 3 :: Int
precedenceEquality = 4 :: Int
precedenceComparison = 5 :: Int
precedenceTerm = 6 :: Int
precedenceFactor = 9 :: Int
precedenceUnary = 10 :: Int
precedenceCall = 11 :: Int
precedencePrimary = 12 :: Int

precedenceOf :: Token -> Int
precedenceOf BangEqual                  = precedenceEquality
precedenceOf EqualEqual                 = precedenceEquality
precedenceOf Frontend.Scan.Greater      = precedenceComparison
precedenceOf Frontend.Scan.GreaterEqual = precedenceComparison
precedenceOf Frontend.Scan.Less         = precedenceComparison
precedenceOf Frontend.Scan.LessEqual    = precedenceComparison
precedenceOf Plus                       = precedenceTerm
precedenceOf Minus                      = precedenceTerm
precedenceOf Star                       = precedenceFactor
precedenceOf Slash                      = precedenceFactor
precedenceOf _                          = 0

infixParse :: Parser -> Located Expr -> Int -> ParserOutput (Maybe (Located Expr))
infixParse parser lhs prec
    | (precedenceOf $ getFirstToken parser) >= prec =
        let locatedOperatorToken@(Located _ operatorToken):_ = tokens parser
            chosenParseFunc = case operatorToken of
                BangEqual                  -> Just parseBinaryExpr
                EqualEqual                 -> Just parseBinaryExpr
                Frontend.Scan.Greater      -> Just parseBinaryExpr
                Frontend.Scan.GreaterEqual -> Just parseBinaryExpr
                Frontend.Scan.Less         -> Just parseBinaryExpr
                Frontend.Scan.LessEqual    -> Just parseBinaryExpr
                Plus                       -> Just parseBinaryExpr
                Minus                      -> Just parseBinaryExpr
                Star                       -> Just parseBinaryExpr
                Slash                      -> Just parseBinaryExpr
                _                          -> Nothing
        in case chosenParseFunc of
                Just func ->
                    let (maybeInfixParsed, infixErrors, afterInfixParser) = func (parser `advance` 1) lhs locatedOperatorToken
                    in case maybeInfixParsed of
                        Just newLhs -> infixParse afterInfixParser newLhs prec
                        Nothing -> (Nothing, infixErrors, afterInfixParser)
                Nothing   -> (Just lhs, [], parser)
    | otherwise = (Just lhs, [], parser)

parseBinaryExpr :: Parser -> Located Expr -> Located Token -> ParserOutput (Maybe (Located Expr))
parseBinaryExpr parser locatedLhs@(Located lhsSpan _) locatedOperator@(Located operatorSpan operatorToken) =
    let (binaryOperator', rhsOf) = case operatorToken of
            Plus                       -> (Add, "addition")
            Minus                      -> (Sub, "subtraction")
            Star                       -> (Mult, "multiplication")
            Slash                      -> (Div, "division")
            BangEqual                  -> (Frontend.Ast.NotEqual, "equality")
            EqualEqual                 -> (Frontend.Ast.Equal, "equality")
            Frontend.Scan.Greater      -> (Frontend.Ast.Greater, "comparison")
            Frontend.Scan.GreaterEqual -> (Frontend.Ast.GreaterEqual, "comparison")
            Frontend.Scan.Less         -> (Frontend.Ast.Less, "comparison")
            Frontend.Scan.LessEqual    -> (Frontend.Ast.LessEqual, "comparison")
            _     -> error "invalid binary operator"
        binaryOperator = Located operatorSpan binaryOperator'
        (maybeRhs, rhsErrors, rhsParser) = parseExpr parser (precedenceOf operatorToken + 1) $ "right hand side to " ++ rhsOf ++ " expression"
        maybeBinary = (\locatedRhs@(Located rhsSpan _) -> Located (lhsSpan `joinSpan` rhsSpan) $ BinaryExpr locatedLhs binaryOperator locatedRhs) <$> maybeRhs
    in (maybeBinary, rhsErrors, rhsParser)

parseTokenExpr :: Parser -> Located Token -> ParserOutput (Maybe (Located Expr))
parseTokenExpr parser (Located tokenSpan token) =
    let expr = case token of
            BoolLiteral bool  -> BoolExpr bool
            NumberLiteral num -> NumberExpr num
            StringLiteral str -> StringExpr str
            NilLiteral -> NilExpr
            _ -> error "parse a bool expr where the first token is not a BoolLiteral"
    in (Just $ Located tokenSpan expr, [], parser)

parseGroupingExpr :: Parser -> Located Token -> ParserOutput (Maybe (Located Expr))
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

parseUnaryExpr :: Parser -> Located Token -> ParserOutput (Maybe (Located Expr))
parseUnaryExpr parser (Located operatorSpan operatorToken) =
    let (operator', unaryExprType) = case operatorToken of
            Bang -> (Not, "logical not")
            Minus -> (Neg, "negation")
            _ -> error $ "parse a unary expr with invalid unary operator: " ++ show operatorToken
        operator = Located operatorSpan operator'
        (maybeOperand, operandErrors, nextParser) = parseExpr parser (precedenceOf operatorToken) $ "operand to " ++ unaryExprType ++ " expression"
        unaryExpr = (\locatedOperand@(Located operandSpan _) -> Located (operatorSpan `joinSpan` operandSpan) $ UnaryExpr operator locatedOperand) <$> maybeOperand
    in (unaryExpr, operandErrors, nextParser)

parseUnaryPositiveExpr :: Parser -> Located Token -> ParserOutput (Maybe (Located Expr))
parseUnaryPositiveExpr parser (Located operatorSpan _) =
    let (_, operandErrors, nextParser) = parseExpr parser precedenceUnary $ "operand to unary + expression"
    in (Nothing, (UnaryPlusUnsupported operatorSpan):operandErrors, nextParser)
