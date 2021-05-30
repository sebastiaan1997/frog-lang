module Main(main) where
    import Lexer
    import Parser

    import Executor
    import qualified Compiler as C
    import qualified Printer as P

    import Debug.Trace ( trace, traceShow )
    import qualified IlCompiler as Il
    import qualified Cpu as C


    main :: IO ()
    main =
        do {
            contents <- readFile "main.fg";
            putStrLn "Token stream:";
            putStrLn "====";
            print (Lexer.lexFrog contents);
            putStrLn "AST:";
            putStrLn "====";
            print ( Parser.parse  (Lexer.lexFrog contents));
            putStrLn "IL:";
            putStrLn "====";
            print (Il.compileOptimize C.armCortexM0  (Parser.parse  (Lexer.lexFrog contents)));
            putStrLn "\n====\n";
            putStrLn "ASM - ARM Cortex M0:";
            putStrLn "====";

            -- putStrLn (show (C.build (snd(Parser.parse  (Lexer.lexFrog contents)))));
            -- putStrLn (P.printAsm  (C.build (snd(Parser.parse  (Lexer.lexFrog contents)))))
        }
