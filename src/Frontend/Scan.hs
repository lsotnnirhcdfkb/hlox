module Frontend.Scan (Token(..), scan) where

import Data.List
import Data.Char(isDigit, isAlpha)
import Frontend.Diagnostic

data Token = OpenParen
           | CloseParen
           | OpenBrace
           | CloseBrace

           | Dot
           | Comma

           | Minus
           | Plus
           | Slash
           | Star

           | Semicolon

           | Bang
           | Equal
           | Less
           | Greater

           | BangEqual
           | EqualEqual
           | LessEqual
           | GreaterEqual

           | Identifier String

           | StringLiteral String
           | NumberLiteral Double
           | BoolLiteral Bool
           | Nil

           | Super
           | This

           | And
           | Or

           | Class
           | Fun

           | If
           | Else
           | For
           | While
           | Print
           | Return
           | Var

           | Eof
           deriving (Show)

data Scanner = Scanner {
        originalSource :: String,
        sourceLeft :: String,
        currentChar :: Int,
        currentLine :: Int,
        currentColumn :: Int
    }
    deriving (Show)

data ScanError = ScanError (Located String)
    deriving (Show)

instance ToError ScanError where
    toErr (ScanError (Located errSpan errMsg)) = Error
        [ Message (Just $ At errSpan) errMsg
        ]

scan :: String -> ([Located Token], [ScanError])
scan source = scan' Scanner {
        originalSource = source,
        sourceLeft = source,
        currentChar = 0,
        currentLine = 1,
        currentColumn = 1
    }

scan' :: Scanner -> ([Located Token], [ScanError])
scan' scanner
    | atEnd scanner   = ([scannerToLocated scanner 0 $ Eof], [])
    | currChar == '(' = tokenAndAdvance scanner 1 $ OpenParen
    | currChar == ')' = tokenAndAdvance scanner 1 $ CloseParen
    | currChar == '{' = tokenAndAdvance scanner 1 $ OpenBrace
    | currChar == '}' = tokenAndAdvance scanner 1 $ CloseBrace
    | currChar == '.' = tokenAndAdvance scanner 1 $ Dot
    | currChar == ',' = tokenAndAdvance scanner 1 $ Comma
    | currChar == '-' = tokenAndAdvance scanner 1 $ Minus
    | currChar == '+' = tokenAndAdvance scanner 1 $ Plus
    | currChar == ';' = tokenAndAdvance scanner 1 $ Semicolon
    | currChar == '*' = tokenAndAdvance scanner 1 $ Star

    | currChar == '!' =
        let (tokenLength, tokenType) = if matchedEq then (2, BangEqual) else (1, Bang)
        in tokenAndAdvance scanner tokenLength tokenType
    | currChar == '=' =
        let (tokenLength, tokenType) = if matchedEq then (2, EqualEqual) else (1, Equal)
        in tokenAndAdvance scanner tokenLength tokenType
    | currChar == '<' =
        let (tokenLength, tokenType) = if matchedEq then (2, LessEqual) else (1, Less)
        in tokenAndAdvance scanner tokenLength tokenType
    | currChar == '>' =
        let (tokenLength, tokenType) = if matchedEq then (2, GreaterEqual) else (1, Greater)
        in tokenAndAdvance scanner tokenLength tokenType

    | currChar == '/' =
        case nextChar of
            Just '/' ->
                let toNl = findIndex (=='\n') $ sourceLeft scanner
                    commentLength = case toNl of
                        Just amtToNl -> amtToNl
                        Nothing      -> length $ sourceLeft scanner
                in scan' $ advanceScanner scanner commentLength
            _ -> tokenAndAdvance scanner 1 Slash

    | currChar == '"' =
        let afterOpenQuote = drop 1 $ sourceLeft scanner
            closingQuote = findIndex (=='"') afterOpenQuote
        in case closingQuote of
                Just amtToQuote -> tokenAndAdvance scanner (amtToQuote + 2) $ StringLiteral $ take amtToQuote afterOpenQuote
                Nothing         -> errorAndAdvance scanner (length $ sourceLeft scanner) $ "unterminated string literal"

    | isDigit currChar =
        let digits = takeWhile isDigit $ sourceLeft scanner
            amtIntDigits = length digits
            hasFloat = (length (sourceLeft scanner) > amtIntDigits) && (sourceLeft scanner !! amtIntDigits == '.') && (isDigit $ sourceLeft scanner !! (amtIntDigits + 1))
            allDigits = if hasFloat
                then
                    let floatingComponent = takeWhile isDigit $ drop (amtIntDigits + 1) $ sourceLeft scanner
                    in digits ++ "." ++ floatingComponent
                else digits
        in tokenAndAdvance scanner (length allDigits) $ NumberLiteral $ read allDigits

    | isAlpha currChar =
        let iden = takeWhile (\ch -> isAlpha ch || ch == '_') $ sourceLeft scanner
            numChars = length iden
            tokenValue = case iden of
                "and" -> And
                "class" -> Class
                "else" -> Else
                "false" -> BoolLiteral False
                "for" -> For
                "fun" -> Fun
                "if" -> If
                "nil" -> Nil
                "or" -> Or
                "print" -> Print
                "return" -> Return
                "super" -> Super
                "this" -> This
                "true" -> BoolLiteral True
                "var" -> Var
                "while" -> While
                _ -> Identifier iden
        in tokenAndAdvance scanner numChars tokenValue

    | currChar == ' ' || currChar == '\r' || currChar == '\t' || currChar == '\n' = scan' $ advanceScanner scanner 1

    | otherwise             = errorAndAdvance scanner 1 "bad character"

    where
        currChar = sourceLeft scanner !! 0 -- if sourceLeft scanner does not have any chars, then the atEnd guard would be chosen and this computation would not be necessary, so it cannot crash
        nextChar = if length (sourceLeft scanner) > 1
            then Just $ sourceLeft scanner !! 1
            else Nothing
        matchedEq = case nextChar of
            Just '=' -> True
            _ -> False

atEnd :: Scanner -> Bool
atEnd (Scanner { sourceLeft = [] }) = True
atEnd _ = False

scannerToLocated :: Scanner -> Int -> a -> Located a
scannerToLocated scanner thingLength = Located Span {
        source = originalSource scanner,
        start = currentChar scanner,
        end = currentChar scanner + thingLength,
        line = currentLine scanner,
        col = currentColumn scanner
    }

tokenAndAdvance :: Scanner -> Int -> Token -> ([Located Token], [ScanError])
tokenAndAdvance scanner tokenLength tokenValue =
    let currentToken = (scannerToLocated scanner tokenLength tokenValue)
        (nextTokens, nextErrs) = scan' $ advanceScanner scanner tokenLength
    in (currentToken:nextTokens, nextErrs)

errorAndAdvance :: Scanner -> Int -> String -> ([Located Token], [ScanError])
errorAndAdvance scanner errLength err =
    let currentErr = ScanError $ scannerToLocated scanner errLength err
        (nextTokens, nextErrs) = scan' $ advanceScanner scanner errLength
    in (nextTokens, currentErr:nextErrs)

advanceScanner :: Scanner -> Int -> Scanner
advanceScanner scanner 0 = scanner
advanceScanner scanner 1 = Scanner {
        originalSource = originalSource scanner,
        sourceLeft = drop 1 $ sourceLeft scanner,
        currentChar = currentChar scanner + 1,
        currentLine = nextLine,
        currentColumn = nextColumn
    }
    where
        pastNl = sourceLeft scanner !! 0 == '\n'
        nextLine = currentLine scanner + (if pastNl then 1 else 0)
        nextColumn = if pastNl then 1 else currentColumn scanner + 1
advanceScanner scanner n = advanceScanner (advanceScanner scanner $ n - 1) 1
