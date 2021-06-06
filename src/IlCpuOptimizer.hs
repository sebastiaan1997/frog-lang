{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module IlCpuOptimizer(fillRegisters) where
    import Debug.Trace (trace, traceShow)
    import qualified Cpu as C
    import qualified Il as I
    import Data.List (elemIndex, (\\))
    import qualified Data.HashMap.Strict as HM
    import qualified Data.Maybe as M

    -- Get free registers of type in the given context.
    getFreeRegistersByType :: C.RegisterType -> C.Cpu -> HM.HashMap Int (I.Reference, Bool) -> [Int]
    getFreeRegistersByType registerType cpu state = (map C.registerIndex . C.getRegistersByType registerType) cpu \\ HM.keys state

    -- | Get registers that are already occupied by data by type.
    getOccupiedLockedRegistersByType :: HM.HashMap Int (I.Reference, Bool) -> [Int]
    getOccupiedLockedRegistersByType  = HM.keys . HM.filter snd 

    -- | Get a free general purpose register, if changes had to be made, the function also returns instructions for that given change.
    getFreeGPRegister :: C.Cpu -> HM.HashMap Int (I.Reference, Bool) -> Maybe (Int, [I.Instruction])
    getFreeGPRegister cpu registers = case (freeRegister, unlockedRegister, unloadRef) of
            (Just registerId, _, _) -> Just (registerId, [])
            (Nothing, Just registerId, Just (ref, _)) -> Just (registerId, [I.StoreFromReg (I.Register registerId, ref)])
            _ -> Nothing
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
    -- | Load a register to a Register.
    loadReference :: I.Reference -> C.Cpu -> HM.HashMap Int (I.Reference, Bool) -> Maybe ((Int, I.Reference, Bool), [I.Instruction])
    -- loadReference I.ReturnValue _ _ = Nothing
    loadReference (I.Variable "") _ _ = Nothing -- Skip empty variables
    loadReference (I.DataLabel _) _ _ = Nothing -- Skip datalabels. It breaks other code.
    -- | Load referenc to register 
    loadReference ref cpu@C.Cpu{C.registers=r} registers = case (reg, emptyRegister) of
        (Just targetRegister, _) -> Just (targetRegister, [])
        (Nothing, Just (r, instr)) -> Just ((r, ref, True), instr ++ [I.LoadToReg (I.Register r, ref)])
        _ -> Nothing
        where
            reg = M.listToMaybe (map (\(x,(y,z)) -> (x,y,z)) (HM.toList (HM.filter (\(d, _) -> d == ref) registers)))
            emptyRegister = getFreeGPRegister cpu registers

    -- | Load reference implementation for folding (left)
    foldlLoadReferenceImpl :: C.Cpu -> ([Maybe I.Data], HM.HashMap Int (I.Reference, Bool), [I.Instruction ]) -> Maybe I.Data -> ([Maybe I.Data], HM.HashMap Int (I.Reference, Bool), [I.Instruction ])

    -- | Ignore return value
    foldlLoadReferenceImpl cpu  (dataList, registerState, instructions) (Just (I.Ref I.ReturnValue)) = (dataList ++ [Just  (I.Ref I.ReturnValue)], registerState, instructions)
    -- | Load the given variable
    foldlLoadReferenceImpl cpu  (dataList, registerState, instructions) (Just (I.Ref r)) = case lrResult of
            (Just ((num, val, lock), instr)) ->  (dataList ++ [(Just . I.Ref . I.R . I.Register) num], HM.insert num (r, lock) registerState, instructions ++ instr )
            Nothing -> (dataList, registerState, instructions)
        where
            lrResult = loadReference r cpu registerState
    -- | Unsupported for now, just ignore and re-add to the datalist.
    foldlLoadReferenceImpl _ (dataList, registerState, instructions) rhs = (dataList ++ [rhs], registerState, instructions)


    -- | Pop variable from registers. Just removes the variable from the register context.
    popVar :: I.Reference -> HM.HashMap Int (I.Reference, Bool) -> HM.HashMap Int (I.Reference, Bool)
    -- | Removes the register directy
    popVar (I.R (I.Register r)) state = HM.delete r state
    -- | Removes the register with a given reference.
    popVar targetRef state =  HM.mapMaybe (\(ref, lock) -> if ref == targetRef then Nothing else Just (ref, lock) ) state

    -- | Fill registers implementation for a foldl function
    fillRegistersImpl ::  C.Cpu -> ([I.Instruction], HM.HashMap Int (I.Reference, Bool)) -> I.Instruction -> ([I.Instruction], HM.HashMap Int (I.Reference, Bool))
    fillRegistersImpl _ (lst, state) i@I.Jump{} = (lst ++ [i], state) -- Ignore 
    fillRegistersImpl _ (lst, state) i@I.B{} = (lst ++ [i], state) -- Ignore 
    fillRegistersImpl _ (lst, state) i@I.BC{} = (lst ++ [i], state) -- Ingore
    fillRegistersImpl _ (lst, state) (I.PopVar vars) = (lst, regs) -- Pop variable from registers.
        where
            regs = foldr popVar state vars
    -- | Routine init, just fill registerstate with arguments that are passed.
    fillRegistersImpl cpu (lst, registerState) i@(I.RoutineInit args) = (lst ++ [i], HM.fromList (zip [0..] (map (\x -> (x, True) ) args)))

    -- | Call, place the 
    fillRegistersImpl cpu (lst, registerState) i@I.Call{I.args=a, I.storeReturn=Just I.ReturnValue} = (lst ++ [i{I.args=M.catMaybes newInstructionData}], newRegisterState)
        where
            (newInstructionData, newRegisterState, newInstructions) = foldl (foldlLoadReferenceImpl cpu) ([], registerState, []) (map Just a)

    -- | Implement call, rearange the registers to match the args and return values.
    fillRegistersImpl cpu (lst, registerState) i@I.Call{I.args=a, I.storeReturn=Just sr} = case head newSr of
        Just (I.Ref  nSR) -> ( lst ++ [I.RToStack saveRegisters , i{I.args=M.catMaybes newInstructionData, I.storeReturn=Just nSR}, I.StackToR saveRegisters], newRegisterState)
        _ -> (lst ++ [i{I.args=M.catMaybes newInstructionData}], newRegisterState)
        where
            (newSr, srState, instr) = foldl (foldlLoadReferenceImpl cpu) ([], registerState, []) [(Just . I.Ref) sr]
            (newInstructionData, newRegisterState, newInstructions) = foldl (foldlLoadReferenceImpl cpu) ([], srState, []) (map Just a)
            occRegs = (map I.Register . getOccupiedLockedRegistersByType) registerState
            saveRegisters = occRegs \\ M.mapMaybe (\case Just (I.Ref (I.R r)) -> Just r ; _ -> Nothing) (newInstructionData ++ newSr)

    -- | Implement call, rearange the registers to mathc the args, just let the returnvalue at position.
    fillRegistersImpl cpu (lst, registerState) i@I.Call{I.args=a, I.storeReturn=Nothing} = (lst ++ [i{I.args=M.catMaybes newInstructionData}], newRegisterState)
        where
            (newInstructionData, newRegisterState, newInstructions) = foldl (foldlLoadReferenceImpl cpu) ([], registerState, []) (map Just a)
    -- | General implementation, Just process all data and references and map them to registers.
    fillRegistersImpl cpu (instructionList, registerState) instruction = (instructionList ++ newInstructions ++ [ M.fromMaybe instruction newInstruction ], newRegisterState)
        where
            instructionData = I.extractInstructionData instruction
            (newInstructionData, newRegisterState, newInstructions) = foldl (foldlLoadReferenceImpl cpu) ([], registerState, []) instructionData
            newInstruction = I.replaceInstructionData instruction newInstructionData


    -- | Replace references by registers by a given CPU.
    fillRegisters :: C.Cpu -> I.ILDocument  -> I.ILDocument
    fillRegisters cpu doc@I.ILDocument{I.instructions =(I.InstructionList lst)}  =  doc{I.instructions =(I.InstructionList . fst .  foldl (fillRegistersImpl cpu) ([], HM.empty)) lst}






