module IlCpuOptimizer where
    import qualified Cpu as C
    import qualified Il as I
    import Data.List (elemIndex)
    import qualified Data.HashMap.Strict as HM
    import qualified Data.Maybe as M


    -- Fills registers and changes some instructions to prepare for a given CPU
    -- optimize :: [I.Structure] -> C.Cpu -> [I.Structure]
    -- optimize


    getFreeGPRegister :: C.Cpu -> HM.HashMap Int (I.Reference, Bool) -> Maybe (Int, [I.Instruction])
    getFreeGPRegister cpu registers = case (freeRegister, unlockedRegister, unloadRef) of
            (Just registerId, _, _) -> Just (registerId, [])
            (Nothing, Just registerId, Just (ref, _)) -> Just (registerId, [I.StoreFromReg (I.Register registerId, ref)])
            _ -> Nothing
            where
                usedUnlockedRegisters = HM.keys (HM.filter (\(_,l) -> not l) registers)
                gpRegisters = C.getRegistersByType C.GeneralPurpose cpu
                registerIds = map C.registerIndex gpRegisters
                freeRegisters = filter (`notElem` usedUnlockedRegisters) registerIds
                freeRegister = M.listToMaybe freeRegisters
                unlockedRegister = M.listToMaybe usedUnlockedRegisters
                unloadRef = case unlockedRegister of
                    Just r ->  HM.lookup r registers
                    Nothing -> Nothing




    loadVariable :: I.Reference -> C.Cpu -> HM.HashMap Int (I.Reference, Bool) -> Maybe ([String], (Int, I.Reference, Bool), [I.Instruction ])
    loadVariable _ _ _ = Nothing
    loadVariable (I.Variable "") _ _ = Nothing
    loadVariable ref cpu@C.Cpu{C.registers=r} registers = case (reg, emptyRegister) of
        (Just targetRegister, _) -> Just (stack, targetRegister, [])
        (Nothing, Just (r, instr)) -> Just (stack, (r, ref, True), instr ++ [I.LoadToReg ((I.Register r, ref))])
        _ -> Nothing
        where
            reg = M.listToMaybe (map (\(x,(y,z)) -> (x,y,z)) (HM.toList (HM.filter (\(d, _) -> d == ref) registers)))
            emptyRegister = getFreeGPRegister cpu registers

    -- fillRegistersOnStrctureImpl :: ([I.Structure], HM.HashMap int (I.Reference , Bool)) -> [I.Structure] -> ([I.Structure], HM.HashMap int (I.Reference , Bool))
    



    fillRegistersOnInstructionImpl :: ([I.Instruction], HM.HashMap int (I.Reference , Bool)) -> I.Instruction -> ([I.Instruction], HM.HashMap int (I.Reference , Bool))


    
    fillRegistersOnInstructionImpl (lst, regs) inst = (lst ++ [inst], regs)
        









    -- functionReturn :: C.Cpu -> [String] -> [(Int, I.Reference)] -> [I.Structure] -> [I.Structure]


    -- fillRegisters







    -- fillRegisters cpu stack regs struct = []





    -- fillRegisters stack rt@I.Routine{I.body=b} = rt{I.body= map stackFunc b}
        -- where stackFunc = fillRegisters stack
    -- fillRegisters stack rt@I.ConditionalBranch{I.condition=c, I.trueBranch=t, I.falseBranch=f} = rt{I.condition=map stackFunc c, I.trueBranch=map stackFunc t, I.falseBranch=map stackFunc f}
        -- where stackFunc = fillRegisters stack
    -- fillRegisters stack b@I.Branch{I.match=m} = b{I.match=stackFunc m}
        -- where stackFunc = fillRegisters stack
    -- fillRegisters stack l@I.FiniteLoop{I.condition=c, I.body=b} =  l{I.body=map stackFunc b, I.condition=map stackFunc c}
        -- where
            -- stackFunc = fillRegisters stack



    -- fillRegisters stack l@I.InfiniteLoop{I.body=b} = l{I.body=map stackFunc b}
        -- where stackFunc = fillRegisters stack
    -- fillRegisters stack (I.Scope s) =  I.Scope (map stackFunc s)
    --     -- where stackFunc = fillRegisters stack
    -- fillRegisters stack vs@I.VariableScope{I.name=n, I.assignment=a, I.body=b} = (vs{I.assignment=map stackFunc a, I.body=map stackFunc b})
    --     where stackFunc = fillRegisters (n:stack)
    -- fillRegisters stack vs@I.ConstantScope{I.assignment=a, I.body=b} = (vs{I.assignment=map stackFunc a, I.body=map stackFunc b})
    --     where stackFunc = fillRegisters stack    