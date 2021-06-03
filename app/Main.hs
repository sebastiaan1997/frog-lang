module Main(main) where
    import Lexer
    import Parser

    import Executor
    import qualified Compiler as C
    
    import qualified ArmCompiler as AC
    import Debug.Trace ( trace, traceShow )
    import qualified IlCompiler as Il
    import qualified Cpu as C
    import System.Environment


    main :: IO ()
    main =
        do {
            args <- getArgs;
            contents <- readFile (head args ++ ".fg");
            putStrLn "Token stream:";
            putStrLn "====";
            print (Lexer.lexFrog contents);
            putStrLn "AST:";
            putStrLn "====";
            print ( Parser.parse  (Lexer.lexFrog contents));
            putStrLn "IL Structure";
            putStrLn "===";
            print (Il.buildStructure (Parser.parse  (Lexer.lexFrog contents)));
            putStrLn "IL:";
            putStrLn "====";
            print (Il.compileOptimize C.armCortexM0  (Parser.parse  (Lexer.lexFrog contents)));
            putStrLn "\n====\n";
            putStrLn "ASM - ARM Cortex M0:";
            putStrLn "====";
            print (AC.compile (Il.compileOptimize C.armCortexM0  (Parser.parse  (Lexer.lexFrog contents))));
            writeFile ("./" ++ head args ++ ".arm") (show (AC.compile (Il.compileOptimize C.armCortexM0  (Parser.parse  (Lexer.lexFrog contents)))))

            -- putStrLn (show (C.build (snd(Parser.parse  (Lexer.lexFrog contents)))));
            -- putStrLn (P.printAsm  (C.build (snd(Parser.parse  (Lexer.lexFrog contents)))))
        }
