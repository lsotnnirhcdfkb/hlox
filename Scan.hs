module Scan (Token(..), scan) where

import Data.List
import Data.Char(isDigit, isAlpha)
import Diagnostic

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
        sourceLeft :: String,
        currentChar :: Int,
        currentLine :: Int,
        currentColumn :: Int
    }
    deriving (Show)

scan :: String -> [Located (Either String Token)]
scan source = scan' Scanner {
        sourceLeft = source,
        currentChar = 0,
        currentLine = 1,
        currentColumn = 1
    }

scan' :: Scanner -> [Located (Either String Token)]
scan' scanner
    | atEnd scanner   = [makeToken scanner 0 $ Right Eof]
    | currChar == '(' = tokenAndAdvance scanner 1 $ Right OpenParen
    | currChar == ')' = tokenAndAdvance scanner 1 $ Right CloseParen
    | currChar == '{' = tokenAndAdvance scanner 1 $ Right OpenBrace
    | currChar == '}' = tokenAndAdvance scanner 1 $ Right CloseBrace
    | currChar == '.' = tokenAndAdvance scanner 1 $ Right Dot
    | currChar == ',' = tokenAndAdvance scanner 1 $ Right Comma
    | currChar == '-' = tokenAndAdvance scanner 1 $ Right Minus
    | currChar == '+' = tokenAndAdvance scanner 1 $ Right Plus
    | currChar == ';' = tokenAndAdvance scanner 1 $ Right Semicolon
    | currChar == '*' = tokenAndAdvance scanner 1 $ Right Star

    | currChar == '!' =
        let (tokenLength, tokenType) = if matchedEq then (2, BangEqual) else (1, Bang)
        in tokenAndAdvance scanner tokenLength $ Right tokenType
    | currChar == '=' =
        let (tokenLength, tokenType) = if matchedEq then (2, EqualEqual) else (1, Equal)
        in tokenAndAdvance scanner tokenLength $ Right tokenType
    | currChar == '<' =
        let (tokenLength, tokenType) = if matchedEq then (2, LessEqual) else (1, Less)
        in tokenAndAdvance scanner tokenLength $ Right tokenType
    | currChar == '>' =
        let (tokenLength, tokenType) = if matchedEq then (2, GreaterEqual) else (1, Greater)
        in tokenAndAdvance scanner tokenLength $ Right tokenType

    | currChar == '/' =
        if nextChar == '/' then
            let toNl = findIndex (=='\n') $ sourceLeft scanner
                commentLength = case toNl of
                    Just amtToNl -> amtToNl
                    Nothing      -> length $ sourceLeft scanner
            in scan' $ advanceScanner scanner commentLength
        else tokenAndAdvance scanner 1 $ Right Slash

    | currChar == '"' =
        let afterOpenQuote = drop 1 $ sourceLeft scanner
            closingQuote = findIndex (=='"') afterOpenQuote
        in case closingQuote of
                Just amtToQuote -> tokenAndAdvance scanner (amtToQuote + 2) $ Right $ StringLiteral $ take amtToQuote afterOpenQuote
                Nothing         -> tokenAndAdvance scanner (length $ sourceLeft scanner) $ Left "unterminated string literal"

    | isDigit currChar =
        let digits = takeWhile isDigit $ sourceLeft scanner
            amtIntDigits = length digits
            hasFloat = sourceLeft scanner !! amtIntDigits == '.' && (isDigit $ sourceLeft scanner !! (amtIntDigits + 1))
            allDigits = if hasFloat
                then
                    let floatingComponent = takeWhile isDigit $ drop (amtIntDigits + 1) $ sourceLeft scanner
                    in digits ++ "." ++ floatingComponent
                else digits
        in tokenAndAdvance scanner (length allDigits) $ Right $ NumberLiteral $ read allDigits

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
        in tokenAndAdvance scanner numChars $ Right tokenValue

    | currChar == ' ' || currChar == '\r' || currChar == '\t' || currChar == '\n' = scan' $ advanceScanner scanner 1

    | otherwise             = tokenAndAdvance scanner 1 $ Left "bad character"

    where
        currChar = sourceLeft scanner !! 0
        nextChar = sourceLeft scanner !! 1
        matchedEq = nextChar == '='

atEnd :: Scanner -> Bool
atEnd (Scanner { sourceLeft = [] }) = True
atEnd _ = False

makeToken :: Scanner -> Int -> a -> Located a
makeToken scanner tokenLength tokenValue = Located {
        value = tokenValue,
        start = currentChar scanner,
        end = currentChar scanner + tokenLength,
        line = currentLine scanner,
        col = currentColumn scanner
    }

tokenAndAdvance :: Scanner -> Int -> Either String Token -> [Located (Either String Token)]
tokenAndAdvance scanner tokenLength tokenValue = (makeToken scanner tokenLength tokenValue) : (scan' $ advanceScanner scanner tokenLength)

advanceScanner :: Scanner -> Int -> Scanner
advanceScanner scanner 0 = scanner
advanceScanner scanner 1 = Scanner {
        sourceLeft = drop 1 $ sourceLeft scanner,
        currentChar = currentChar scanner + 1,
        currentLine = nextLine,
        currentColumn = nextColumn
    }
    where
        pastNl = sourceLeft scanner !! 0 == '\n'
        nextLine = currentLine scanner + (if pastNl then 1 else 0)
        nextColumn = if pastNl then 1 else currentColumn scanner + 1
advanceScanner scanner n = advanceScanner advanced1Less 1
    where
        advanced1Less = advanceScanner scanner (n - 1)
