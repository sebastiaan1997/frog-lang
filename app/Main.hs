module Main(main) where

import Lexer 
import Parser
import Executor
import qualified Compiler as C
import qualified Printer as P


main :: IO ()
main = 
    do {
        contents <- readFile "main.fg";
        putStrLn "AST:";
        putStrLn "====";
        putStrLn (show (snd(Parser.parse  (Lexer.lexFrog contents))));
        putStrLn "\n====\n";
        putStrLn "ASM - ARM Cortex M0:";
        putStrLn "====";
        putStrLn (show (C.build (snd(Parser.parse  (Lexer.lexFrog contents)))));
        putStrLn (P.printAsm  (C.build (snd(Parser.parse  (Lexer.lexFrog contents)))))
    }