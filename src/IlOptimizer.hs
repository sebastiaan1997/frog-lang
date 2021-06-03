module IlOptimizer(optimizeStack) where
    import qualified Il as I


    optimize :: I.InstructionList -> I.InstructionList
    optimize (I.InstructionList []) = I.InstructionList []
    optimize (I.InstructionList lst) = I.InstructionList lst


    optimizeStackFoldlRImpl :: I.Instruction -> [I.Instruction] -> [I.Instruction]
    optimizeStackFoldlRImpl (I.PopVar l) (I.PopVar r :xs) = I.PopVar (l ++ r) : xs
    optimizeStackFoldlRImpl inst list = inst : list



    optimizeStack :: [I.Instruction] ->  [ I.Instruction ]
    optimizeStack = foldr optimizeStackFoldlRImpl [] 


    -- optimizeForCpu :: [I.Instruction] -> Cpu -> [I.Instruction]




