module Frontend.Parse (parse) where

import Frontend.Scan
import Frontend.Ast
import Frontend.Diagnostic

import Control.Monad.State
import Debug.Trace (trace)

-- note: this parser code is kind of janky

-- data {{{1
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
    , errors :: [ParseError]
    } deriving (Show)

type ParserState = State Parser

-- parse {{{1
parse :: [Located Token] -> ([Located Stmt], [ParseError])
parse tokens = (outputAST, outErrs)
    where
        (outputAST, finalParser) = runState parseDecls $ Parser tokens []
        outErrs = errors finalParser

-- helpers {{{1
advance :: Int -> ParserState ()
advance n = state $ \ (Parser tokens errors) -> ((), Parser (drop n tokens) errors)

peek :: ParserState (Located Token)
peek = state $ \ parser -> (head $ tokens parser, parser)

synchronize :: ParserState ()
synchronize =
    getFirstToken >>= \ first ->
    case first of
        Eof -> return ()
        Class -> return ()
        Fun -> return ()
        Var -> return ()
        For -> return ()
        If -> return ()
        While -> return ()
        Print -> return ()
        Return -> return ()
        Semicolon -> advance 1
        _ -> synchronize

getFirstToken :: ParserState Token
getFirstToken = peek >>= \ (Located _ tok) -> return tok

addErrs :: [ParseError] -> ParserState ()
addErrs newErrs = state $
    \ st -> ((), st { errors = errors st ++ newErrs })

-- parsing {{{1
-- decl list {{{2
parseDecls :: ParserState [Located Stmt]
parseDecls =
    getFirstToken >>= \ firstToken ->
    case firstToken of
        Eof -> return []
        _ ->
            parseDecl >>= \ maybeDecl ->
            case maybeDecl of
                Just s ->
                    parseDecls >>= \ nextDecls ->
                    return $ s:nextDecls

                Nothing ->
                    synchronize >>
                    parseDecls >>= \ nextDecls ->
                    return $ nextDecls
-- decl {{{2
parseDecl :: ParserState (Maybe (Located Stmt))
parseDecl =
    getFirstToken >>= \ firstToken ->
    case firstToken of
        Var -> parseVarDecl
        _ -> parseStmt
-- var decl {{{2
parseVarDecl :: ParserState (Maybe (Located Stmt))
parseVarDecl =
    peek >>= \ (Located varSpan _) ->
    advance 1 >>
    peek >>= \ shouldBeName ->
    case shouldBeName of
        Located nameSpan (Identifier name) ->
            -- consume name
            advance 1 >>
            getFirstToken >>= \ maybeIsEqual ->
            (case maybeIsEqual of
                Frontend.Scan.Equal ->
                    parseExpr 0 "variable initializer" >>= \ exprres ->
                    case exprres of
                        Just e@(Located esp _) -> return (esp, Just e)
                        Nothing -> return (nameSpan, Nothing)
                _ -> return (nameSpan, Nothing)) >>= \ (lastSpan, maybeInitializer) ->
            peek >>= \ maybeSemi ->
            case maybeSemi of
                Located semiSpan Semicolon ->
                    advance 1 >>
                    (return $ Just $ Located (varSpan `joinSpan` semiSpan) $ VarStmt (Located nameSpan name) maybeInitializer)
                _ ->
                    addErrs [Expected (After lastSpan) "';'" Nothing] >>
                    return Nothing
        _ ->
            addErrs [Expected (After varSpan) "variable name" Nothing] >>
            return Nothing
-- stmt {{{2
parseStmt :: ParserState (Maybe (Located Stmt))
parseStmt =
    getFirstToken >>= \ first ->
    case first of
        Print -> parsePrintStmt
        _ -> parseExprStmt
-- print stmt {{{2
parsePrintStmt :: ParserState (Maybe (Located Stmt))
parsePrintStmt =
    peek >>= \ (Located printSpan _) ->
    advance 1 >>
    (parseExpr 0 "expression to print" >>= \ maybeExpr ->
    case maybeExpr of
        Just e@(Located esp _) -> return (esp, Just e)
        Nothing -> return (printSpan, Nothing)
    ) >>= \ (lastSpan, maybeExpr) ->
    case maybeExpr of
        Just e ->
            peek >>= \ shouldBeSemi ->
            case shouldBeSemi of
                Located semiSpan Semicolon ->
                    advance 1 >>
                    (return $ Just $ Located (printSpan `joinSpan` semiSpan) $ PrintStmt e)

                _ ->
                    addErrs [Expected (After lastSpan) "';'" Nothing] >>
                    return Nothing

        Nothing ->
            return Nothing
-- expr stmt {{{2
parseExprStmt :: ParserState (Maybe (Located Stmt))
parseExprStmt =
    parseExpr 0 "expression statement" >>= \ maybeExpr ->
    case maybeExpr of
        Just locatedExpr@(Located exprSpan _) ->
            peek >>= \ shouldBeSemi ->
            case shouldBeSemi of
                Located semiSpan Semicolon ->
                    advance 1 >>
                    (return $ Just $ Located (exprSpan `joinSpan` semiSpan) $ ExprStmt locatedExpr)
                _ ->
                    addErrs [Expected (After exprSpan) "';'" Nothing] >>
                    return Nothing
        Nothing -> return Nothing
-- expr {{{2
parseExpr :: Int -> String -> ParserState (Maybe (Located Expr))
parseExpr prec representing =
    prefixParse representing >>= \ prefixParsed ->
    case prefixParsed of
        Just e -> infixParse e prec
        Nothing -> return Nothing
-- prefix parsing {{{2
prefixParse :: String -> ParserState (Maybe (Located Expr))
prefixParse representing =
    peek >>= \ (Located firstSpan first) ->
    case first of
        BoolLiteral _   -> parseTokenExpr
        NumberLiteral _ -> parseTokenExpr
        StringLiteral _ -> parseTokenExpr
        NilLiteral      -> parseTokenExpr
        Identifier _    -> parseTokenExpr
        OpenParen       -> parseGroupingExpr
        Minus           -> parseUnaryExpr
        Bang            -> parseUnaryExpr
        Plus            -> parseUnaryPositiveExpr
        _               ->
            addErrs [Expected (At firstSpan) "expression" $ Just representing] >>
            return Nothing
-- infix parsing {{{2
-- precedence thingies {{{3
precedenceAssign, precedenceOr, precedenceAnd, precedenceEquality, precedenceComparison, precedenceTerm, precedenceFactor, precedenceUnary, precedenceCall, precedencePrimary :: Int
precedenceAssign = 1
precedenceOr = 2
precedenceAnd = 3
precedenceEquality = 4
precedenceComparison = 5
precedenceTerm = 6
precedenceFactor = 7
precedenceUnary = 8
precedenceCall = 9
precedencePrimary = 10

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
-- }}}
infixParse :: Located Expr -> Int -> ParserState (Maybe (Located Expr))
infixParse lhs prec =
    getFirstToken >>= \ first ->
    if precedenceOf first >= prec
    then
        let chosenParseFunc = case first of
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
                    func lhs >>= \ maybeInfixParsed ->
                    case maybeInfixParsed of
                        Just newLhs -> infixParse newLhs prec
                        Nothing -> return Nothing
                Nothing -> return $ Just lhs
    else return $ Just lhs
-- binary expr {{{2
parseBinaryExpr :: Located Expr -> ParserState (Maybe (Located Expr))
parseBinaryExpr locatedLhs@(Located lhsSpan _) =
    peek >>= \ (Located operatorSpan operatorToken) ->
    advance 1 >>
    let (binaryOperator, rhsOf) = case operatorToken of
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
        locatedOperator = Located operatorSpan binaryOperator
    in (parseExpr (precedenceOf operatorToken + 1) $ "right hand side to " ++ rhsOf ++ " expression") >>= \ maybeRhs ->
    return $ (\ locatedRhs@(Located rhsSpan _) -> Located (lhsSpan `joinSpan` rhsSpan) $ BinaryExpr locatedLhs locatedOperator locatedRhs) <$> maybeRhs
-- token expr {{{2
parseTokenExpr :: ParserState (Maybe (Located Expr))
parseTokenExpr =
    peek >>= \ (Located tokenSpan token) ->
    advance 1 >>
    let expr = case token of
            BoolLiteral bool  -> BoolExpr bool
            NumberLiteral num -> NumberExpr num
            StringLiteral str -> StringExpr str
            NilLiteral -> NilExpr
            Identifier name -> VarExpr name
            _ -> error "parse a bool expr where the first token is not a BoolLiteral"
    in return $ Just $ Located tokenSpan expr
-- grouping expr {{{2
parseGroupingExpr :: ParserState (Maybe (Located Expr))
parseGroupingExpr =
    peek >>= \ (Located openParenSpan _) ->
    advance 1 >>
    parseExpr 0 "grouped expression inside parentheses" >>= \ maybeExpr ->
    case maybeExpr of
        Just locatedExpr@(Located exprSpan _) ->
            peek >>= \ shouldBeCloseParen ->
            case shouldBeCloseParen of
                Located closeParenSpan CloseParen ->
                    advance 1 >>
                    (return $ Just $ Located (openParenSpan `joinSpan` closeParenSpan) $ GroupingExpr locatedExpr)
                _ ->
                    addErrs [ExpectedCloseParen exprSpan openParenSpan] >>
                    return Nothing
        Nothing -> return Nothing
-- unary expr {{{2
parseUnaryExpr :: ParserState (Maybe (Located Expr))
parseUnaryExpr =
    peek >>= \ (Located operatorSpan operatorToken) ->
    advance 1 >>
    let (operator, operandName) = case operatorToken of
            Bang -> (Not, "logical not")
            Minus -> (Neg, "negation")
            _ -> error $ "parse a unary expr with invalid unary operator: " ++ show operatorToken
        locatedOperator = Located operatorSpan operator
    in parseExpr precedenceUnary ("operand to " ++ operandName ++ " expression") >>= \ maybeOperand ->
    case maybeOperand of
        Just locatedOperand@(Located operandSpan _) -> return $ Just $ Located (operatorSpan `joinSpan` operandSpan) $ UnaryExpr locatedOperator locatedOperand
        Nothing -> return Nothing
-- unary positive error production {{{2
parseUnaryPositiveExpr :: ParserState (Maybe (Located Expr))
parseUnaryPositiveExpr =
    peek >>= \ (Located plusSpan _) ->
    advance 1 >>
    parseExpr precedenceUnary "operand to unary + expression" >>
    addErrs [UnaryPlusUnsupported plusSpan] >>
    return Nothing
