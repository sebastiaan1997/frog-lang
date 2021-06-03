{-# LANGUAGE LambdaCase #-}
module ArmCompiler(compile) where
    import qualified Il as I
    import qualified Arm as A
    import qualified Data.HashMap.Strict as HM
    import Debug.Trace(trace)
    import Data.List((\\))

    -- | Converts a IL Register to a ARM Register.
    convertRegister :: I.Register -> A.ArmRegister
    convertRegister (I.Register r) = [A.R0, A.R1, A.R2, A.R3, A.R4, A.R5, A.R6, A.R7, A.R8, A.R9, A.R10, A.R11, A.R12] !! r


    -- | Converts IL Datatypes to ARM Datatypes
    convertData :: I.Data -> Either A.ArmRegister A.Data
    convertData (I.Ref (I.R reg)) = (Left . convertRegister) reg --Convert Integer Regster from IL to ARM register.
    convertData (I.BooleanValue val) = (Right . A.UnsignedIntegerValue) (if val then 1 else 0) -- C
    convertData (I.Ref (I.Label lbl)) = (Right . A.LabelReference) lbl
    convertData (I.IntegerValue int) = (Right . A.IntegerValue) int
    convertData (I.Ref (I.StackPointerOffset off)) = (Right . A.RelativePointer)(A.SP, off)
    convertData (I.Ref (I.Absolute ptr)) = (Right . A.Pointer) ptr
    convertData d = (trace . show) d ((Right . A.Pointer) 0)

    compileImpl :: I.Instruction -> ([Either A.Instruction String] , HM.HashMap String A.Data) -> ([Either A.Instruction String], HM.HashMap String A.Data)
    compileImpl  (I.Lbl lbl) (rhs, c) = (Right lbl : rhs, c)

    compileImpl  (I.Increment (I.R r)) (rhs, c) = (Left (A.ADD(armR, armR, Right (A.IntegerValue 1))) : rhs, c)
        where armR = convertRegister r

    compileImpl  (I.Decrement (I.R r)) (rhs, c) = (Left (A.SUB(armR, armR, Right (A.IntegerValue 1))) : rhs, c)
        where armR = convertRegister r

    compileImpl  (I.Decrement (I.R r)) (rhs, c) = (Left (A.SUB(armR, armR, Right (A.IntegerValue 1))) : rhs, c)
        where armR = convertRegister r



    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=I.Ref (I.R r)}  (rhs, c) = (Left (A.CMP(armRL, Left armRR)) : rhs, c)
        where
            armRL = convertRegister l
            armRR = convertRegister r

    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=I.IntegerValue v}  (rhs, c)
        | v < 255 = (Left (A.CMP(armRL, (Right . A.IntegerValue) v)) : rhs, c)
        | otherwise = (rhs, c)
        where
            armRL = convertRegister l

    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=I.BooleanValue  v}  (rhs, c)  = (Left (A.CMP(armRL, (Right . A.IntegerValue) (if v then 1 else 0))) : rhs, c)
        where
            armRL = convertRegister l

    compileImpl I.Sub{I.store=I.R store, I.lhs=I.Ref (I.R lhs), I.rhs=I.Ref (I.R r)} (rhs, c) = ((Left . A.SUB)  (convertRegister store,  convertRegister lhs, (Left . convertRegister) r): rhs , c)

    compileImpl  (I.Goto (I.Label lbl)) (rhs, c) = (Left (A.B lbl) : rhs, c)
    compileImpl (I.RoutineInit args) (rhs, c) = ((Left . A.PUSH) [A.LR] : rhs, c)
    compileImpl I.RetNone (rhs, c) = ((Left . A.POP) [A.PC] : rhs, c)
    compileImpl I.Call{I.storeReturn=Nothing, I.routine=I.Label rt, I.args=[]} (rhs, c) = ((Left . A.PUSH) [A.R0, A.R1, A.R2] : (Left . A.BL) rt : (Left . A.POP) [A.R0, A.R1, A.R2] : rhs, c)
    compileImpl I.BC{I.conditionType=ct, I.target=I.Label t } (rhs, c) = ((Left . branchInstruction) t : rhs, c)
        where
            branchInstruction = case ct of
                I.NotEqual -> A.BNE
                I.Equal -> A.BEQ
                I.Unconditional -> A.B
                I.Greater -> A.BGT
                I.GreaterEqual -> A.BGE
                I.Lesser -> A.BLT
                I.LesserEqual -> A.BLE

    -- compileImpl (I.PopVar p) (rhs, c) = ((Left . branchInstruction) t : rhs, c) 

    compileImpl I.PushVar{I.store=I.R reg, I.source=I.IntegerValue i} (rhs, c)
        | i < 256 = ((Left . A.MOV) (convertRegister reg, (Right . A.IntegerValue) i) : rhs, c)
        | otherwise = ((Left . A.LDR) (convertRegister reg, (Right . A.LabelReference ) valueLbl) : rhs, HM.insert valueLbl (A.IntegerValue i) c)
        where valueLbl = "VAL_"  ++ show i
    compileImpl I.PushVar{I.store=I.R reg, I.source=I.BooleanValue b} (rhs, c) =
        ((Left . A.MOV) (convertRegister reg, (Right . A.IntegerValue) (if b then 1 else 0)) : rhs, c)

    compileImpl (I.Ret(I.Ref (I.R (I.Register reg)))) (rhs, c)
        | reg < 9 = ((Left . A.MOV) (A.R0, (Left . convertRegister) (I.Register reg)) : rhs, c)

    compileImpl (I.Ret(I.BooleanValue b)) (rhs, c)
        = ((Left . A.MOV) (A.R0, (Right . A.IntegerValue) (if b then 1 else 0)) : (Left . A.POP) [A.PC] : rhs, c)

    compileImpl (I.Ret(I.IntegerValue v)) (rhs, c)
        = ((Left . A.MOV) (A.R0, (Right . A.IntegerValue) v) : rhs, c)

    compileImpl (I.StackToR regs) (rhs, c) =  ((Left . A.POP) ((map convertRegister . filter (\case (I.Register r) -> r < 4; _->False )) regs) : rhs, c)
    compileImpl (I.RToStack  regs) (rhs, c) = ((Left . A.PUSH) ((map convertRegister . filter (\case (I.Register r) -> r < 4; _->False )) regs) : rhs, c)

    compileImpl I.Call{I.storeReturn=Just I.ReturnValue,I.routine=I.Label rt , I.args=p} (rhs, c)
        = ( argFill ++ [(Left . A.BL) rt, (Left . A.POP)[A.PC] ] ++ rhs, c)
        where
            argFill = case p of
                [_] -> []
                (x: xs) -> map Left (zipWith (curry mapArgMoveInstruction) (map (convertRegister . I.Register) [1..]) xs)
                [] -> []


    compileImpl I.Call{I.storeReturn=Just (I.R retTarget),I.routine=I.Label rt , I.args=p} (rhs, c)
        = ( preCall ++ ((Left . A.BL) rt : postCall) ++ rhs, c)
        where
            argFill = map Left (zipWith (curry mapArgMoveInstruction) (map (convertRegister . I.Register) [0..]) p)
            retReg = convertRegister retTarget
            preCall = argFill
            postCall = (Left . A.MOV) (convertRegister retTarget, Left A.R0) : rhs



    compileImpl lhs (rhs, c) = trace ("Failed -> " ++ show lhs )(rhs, c)

    mapArgMoveInstruction :: (A.ArmRegister, I.Data) -> A.Instruction
    mapArgMoveInstruction (reg, I.Ref(I.R r)) = A.MOV (reg, (Left . convertRegister) r)






    compile :: I.InstructionList -> A.ArmInstructions
    compile (I.InstructionList lst) = (A.ArmInstructions . fst . foldr compileImpl ([], HM.empty)) lst









