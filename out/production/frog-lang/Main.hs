module Main(main) where

import Lexer 
import Parser
import Executor


main :: IO ()
main = 
    do {
        contents <- readFile "main.fg";
        Executor.execute (snd(Parser.parse  (Lexer.lexFrog contents)))


    }

