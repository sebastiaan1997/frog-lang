module Printer(printAsm) where
    import qualified Compiler as C

    printAsm :: [C.ArmInstruction] -> String
    printAsm asm = foldl1 (++) (map printLine asm)

    printLine :: C.ArmInstruction -> String
    printLine (C.LABEL lbl) = "." ++ lbl ++ ":\n" 
    printLine (C.MOV (lhs, rhs)) = "\tMOV " ++ show lhs ++ "," ++ show rhs ++ "\n"
    printLine (C.CMP (lhs, rhs)) = "\tCMP " ++ show lhs ++ "," ++ show rhs ++ "\n"
    printLine (C.ADD (lhs, rhs, rhs2)) = "\tADD " ++ show lhs ++ "," ++ show rhs ++ "," ++ show rhs2 ++ "\n"
    printLine instruction = "\t"++ show instruction ++ "\n"

    


    -- printAsm :: 

