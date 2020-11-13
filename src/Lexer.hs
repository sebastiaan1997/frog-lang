module Lexer(lexFrog,FrogToken(..)) where

import Data.Char ( isDigit )
data FrogToken = Identifier String | DigitLiteral String | StringLiteral String | Other Char




-- lexDigit :: String -> Integer -> [ FrogToken ]


lexDigit :: String -> String -> [ FrogToken ]
lexDigit [] [] = []
lexDigit [] buffer = [ DigitLiteral buffer ]
lexDigit (x: xs) buffer | isDigit x = lexDigit xs (buffer ++ [x])
lexDigit (x: xs) buffer = DigitLiteral buffer : lexFrog (x:xs)



lexString :: String -> String -> [ FrogToken ]

lexString [] [] = []
lexString [] buffer  = [StringLiteral buffer]
lexString (x : xs) buffer | x == '"' = StringLiteral buffer : (lexFrog xs)
lexString (x : xs) buffer = lexString xs (buffer ++ [x])



lexIdentifier :: String -> String -> [FrogToken]
lexIdentifier [] [] = []
lexIdentifier [] buffer = [ Identifier buffer ]
lexIdentifier (x : xs) buffer| x == ' ' = Identifier buffer : (lexFrog xs)



lexFrog :: String -> [ FrogToken ]
lexFrog [] = []
lexFrog (x : xs) | isDigit x = lexDigit (x : xs) ""
lexFrog (x : xs) | x == '"' = lexString xs ""
lexFrog (x : xs) = lexIdentifier (x : xs) ""

