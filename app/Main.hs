module Main(main) where

import Lexer 


printLexer :: [Lexer.FrogToken] -> IO ()
printLexer (Lexer.Other(x): xs) = do
    print ("Other(" ++ [ x ] ++ ")")
    printLexer xs

printLexer (Lexer.DigitLiteral(x): xs) = do
    print ("DigitLiteral(" ++ x ++ ")")
    printLexer xs

printLexer (Lexer.StringLiteral(x): xs) = do
    print ("StringLiteral(" ++ x ++ ")")
    printLexer xs

printLexer [] = print "---"


main :: IO ()



main = printLexer (Lexer.lexFrog "\"Hello World!\" 105")

