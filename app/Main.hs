module Main(main) where

import Lexer 
import Parser



statement :: String
statement = "const name = \"Sebastiaan\"; print(name); return name;"

main :: IO ()
main = do {
        print tokenStream;
        print ast
    }
    where tokenStream = Lexer.lexFrog statement; ast = Parser.parse tokenStream
    
