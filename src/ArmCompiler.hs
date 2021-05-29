module ArmCompiler(compile) where
    import qualified Il as I
    import qualified Arm as A

    compile :: I.InstructionList -> [A.Instruction]
    compile (I.InstructionList l) = snd (foldl compileImpl ([], []) l) -- snd (compileImpl ([], l))



    compileImpl :: ([String], [A.Instruction]) -> I.Instruction -> ([String], [A.Instruction])
    compileImpl (stack, instr) (I.Lbl lbl) = (stack, instr ++ [A.LABEL lbl])
    compileImpl (stack, instr) (I.Goto (I.Label lbl)) = (stack, instr ++ [A.B lbl])
    -- compileImpl (stack, instr) (I.Cmp Data Data) =




    