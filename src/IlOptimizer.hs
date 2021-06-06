module IlOptimizer(optimizeStack) where
    import qualified Il as I

    -- | Execution entry for some general IL optimizations.
    optimize :: I.InstructionList -> I.InstructionList
    optimize (I.InstructionList []) = I.InstructionList []
    optimize (I.InstructionList lst) = I.InstructionList lst

    -- | Group stack pops
    optimizeStackFoldlRImpl :: I.Instruction -> [I.Instruction] -> [I.Instruction]
    optimizeStackFoldlRImpl (I.PopVar l) (I.PopVar r :xs) = I.PopVar (l ++ r) : xs
    optimizeStackFoldlRImpl inst list = inst : list


    -- | Group stack pops
    optimizeStack :: I.ILDocument -> I.ILDocument 
    optimizeStack doc@I.ILDocument{I.instructions=I.InstructionList i} = doc{I.instructions=I.InstructionList (foldr optimizeStackFoldlRImpl [] i)}


    -- optimizeForCpu :: [I.Instruction] -> Cpu -> [I.Instruction]




