module IlCpuOptimizer(fillRegisters) where
    import Data.List
    import Debug.Trace (trace, traceShow)
    import qualified Cpu as C
    import qualified Il as I
    import Data.List (elemIndex)
    import qualified Data.HashMap.Strict as HM
    import qualified Data.Maybe as M


    -- Fills registers and changes some instructions to prepare for a given CPU
    -- optimize :: [I.Structure] -> C.Cpu -> [I.Structure]
    -- optimize


    getFreeGPRegister :: C.Cpu -> HM.HashMap Int (I.Reference, Bool) -> Maybe (Int, [I.Instruction])
    getFreeGPRegister cpu registers = trace ("[getFreeGPRegister] => freeRegister: " ++ show freeRegister)  (case (freeRegister, unlockedRegister, unloadRef) of
            (Just registerId, _, _) -> Just (registerId, [])
            (Nothing, Just registerId, Just (ref, _)) -> Just (registerId, [I.StoreFromReg (I.Register registerId, ref)])
            _ -> Nothing)
            where
                usedUnlockedRegisters = HM.keys (HM.filter (\(_,l) -> not l) registers)
                gpRegisters = C.getRegistersByType C.GeneralPurpose cpu
                registerIds = map C.registerIndex gpRegisters
                freeRegisters = registerIds \\ HM.keys registers
                freeRegister = M.listToMaybe freeRegisters
                unlockedRegister = M.listToMaybe usedUnlockedRegisters
                unloadRef = case unlockedRegister of
                    Just r ->  HM.lookup r registers
                    Nothing -> Nothing




    loadReference :: I.Reference -> C.Cpu -> HM.HashMap Int (I.Reference, Bool) -> Maybe ((Int, I.Reference, Bool), [I.Instruction])
    loadReference (I.Variable "") _ _ = Nothing
    loadReference ref cpu@C.Cpu{C.registers=r} registers = case (reg, emptyRegister) of
        (Just targetRegister, _) -> Just (targetRegister, [])
        (Nothing, Just (r, instr)) -> Just ((r, ref, True), instr ++ [I.LoadToReg (I.Register r, ref)])
        _ -> Nothing
        where
            reg = M.listToMaybe (map (\(x,(y,z)) -> (x,y,z)) (HM.toList (HM.filter (\(d, _) -> d == ref) registers)))
            emptyRegister = getFreeGPRegister cpu registers


    foldlLoadReferenceImpl :: C.Cpu -> ([Maybe I.Data], HM.HashMap Int (I.Reference, Bool), [I.Instruction ]) -> Maybe I.Data -> ([Maybe I.Data], HM.HashMap Int (I.Reference, Bool), [I.Instruction ])
    foldlLoadReferenceImpl cpu  (dataList, registerState, instructions) (Just (I.Ref r)) = case lrResult of
            (Just ((num, val, lock), instr)) ->  (dataList ++ [(Just . I.Ref . I.R . I.Register) num], HM.insert num (r, lock) registerState, instructions ++ instr )
            Nothing -> (dataList, registerState, instructions)
        where
            lrResult = loadReference r cpu registerState

    foldlLoadReferenceImpl _ (dataList, registerState, instructions) rhs = (dataList ++ [rhs], registerState, instructions)






    fillRegistersImpl ::  C.Cpu -> ([I.Instruction], HM.HashMap Int (I.Reference, Bool)) -> I.Instruction -> ([I.Instruction], HM.HashMap Int (I.Reference, Bool))
    fillRegistersImpl _ (lst, state) i@I.PushVar{} = (lst ++ [i], state)
    fillRegistersImpl _ (lst, state) i@I.PopVar{} = (lst ++ [i], state)
    fillRegistersImpl _ (lst, state) i@I.Jump{} = (lst ++ [i], state)


    fillRegistersImpl cpu (instructionList, registerState) instruction = traceShow registerState (instructionList ++ newInstructions ++ [ M.fromMaybe instruction newInstruction ], newRegisterState)
        where
            instructionData = I.extractInstructionData instruction
            (newInstructionData, newRegisterState, newInstructions) = foldl (foldlLoadReferenceImpl cpu) ([], registerState, []) instructionData
            newInstruction = I.replaceInstructionData instruction newInstructionData

    fillRegisters :: C.Cpu -> I.InstructionList -> I.InstructionList
    fillRegisters cpu (I.InstructionList lst)  =  (I.InstructionList . fst .  foldl (fillRegistersImpl cpu) ([], HM.empty)) lst


    



