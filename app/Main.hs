module Main(main) where

import Lexer 
import Parser


main :: IO ()
main = 
    do {
        contents <- readFile "main.fg";
        print (Parser.parse  (Lexer.lexFrog contents))
    }

