module Lexer(lexFrog,FrogToken(..)) where

import Data.Char (isDigit, isSpace)



data FrogToken = Identifier String | DigitLiteral String | StringLiteral String | Keyword String | Other Char | Operator String | SLComment String | MLComment String | DocComment String | BlockOpen Char  | BlockClose Char | None
    deriving (Eq, Show)





blockOpen :: [Char]
blockOpen = "{[("

isBlockOpen :: Char -> Bool
isBlockOpen ch = ch `elem` blockOpen

blockClose :: [Char]
blockClose = ")]}"

isBlockClose :: Char -> Bool
isBlockClose ch = ch `elem` blockClose

operators :: String
operators = "+-*/^&%!~="

keywords :: [ String ]
keywords = [ "const", "let", "fn", "rt"]


isOperator :: Char -> Bool
isOperator ch = ch `elem` operators

isKeyword :: String -> Bool
isKeyword str = str `elem` keywords




lexOperator :: String -> String -> [ FrogToken ]
lexOperator [] [] = []
lexOperator [] buffer = [ Operator buffer ]
lexOperator (x : xs) buffer | isOperator x = lexOperator xs (buffer ++ [x])
lexOperator input buffer =  Operator buffer : lexFrog input

lexDigit :: String -> String -> [ FrogToken ]
lexDigit [] [] = []
lexDigit [] buffer = [ DigitLiteral buffer ]
lexDigit (x: xs) buffer | isDigit x = lexDigit xs (buffer ++ [x])
lexDigit (x: xs) buffer = DigitLiteral buffer : lexFrog (x:xs)

lexString :: String -> String -> [ FrogToken ]
lexString [] [] = []
lexString [] buffer  = [StringLiteral buffer]
lexString (x : xs) buffer | x == '"' = StringLiteral buffer : lexFrog xs
lexString (x : xs) buffer = lexString xs (buffer ++ [x])


lexIdentifier :: String -> String -> [FrogToken]
lexIdentifier [] [] = []
lexIdentifier [] buffer = [ Identifier buffer ]
lexIdentifier (x : xs) buffer | isSpace x = Identifier buffer : lexFrog xs
lexIdentifier (x : xs) buffer | isSpace x && isKeyword buffer = Identifier buffer : lexFrog xs
lexIdentifier (x : xs) buffer = lexIdentifier xs (buffer ++ [x])


lexSLComment:: String -> String -> [FrogToken]
lexSLComment ('\n': xs) buffer = SLComment buffer : lexFrog xs
lexSLComment (x: xs) buffer = lexSLComment xs (buffer ++ [x])

lexMLComment:: String -> String -> [FrogToken]
lexMLComment ('*' : '/': xs) buffer = MLComment buffer : lexFrog xs
lexMLComment (x : xs) buffer = lexMLComment xs (buffer ++ [x])

lexDocComment :: String -> String -> [FrogToken]
lexDocComment ('*' : '/': xs) buffer = MLComment buffer : lexFrog xs
lexDocComment (x : xs) buffer = lexDocComment xs (buffer ++ [x])

lexFrog :: String -> [ FrogToken ]
lexFrog [] = []
lexFrog ('/' : '/' : xs) = lexSLComment xs ""
lexFrog ('/' : '*' : '*' : xs) = lexDocComment xs ""
lexFrog ('/' : '*' : xs) = lexMLComment xs ""
lexFrog (x : xs) | isSpace x = lexFrog xs
lexFrog (x : xs) | isDigit x = lexDigit (x : xs) ""
lexFrog (x : xs) | isOperator x = lexOperator xs [x]
lexFrog (x : xs) | isBlockOpen x =  BlockOpen x : lexFrog xs
lexFrog (x : xs) | isBlockClose x  = BlockClose x : lexFrog xs
lexFrog ('"' : xs) = lexString xs ""
lexFrog (x : xs) = lexIdentifier (x : xs) ""
