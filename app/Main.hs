module Main(main) where

import Lexer 



main :: IO ()
main = print (Lexer.lexFrog "// This is a name assignment \n const name = \"Sebastiaan\" ")

