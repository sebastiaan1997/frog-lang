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
    convertData (I.BooleanValue val) = (Right . A.UnsignedIntegerValue) (if val then 1 else 0) -- Convert a boolean value to a unsigned integer value of 1 when true or 0 when false
    convertData (I.Ref (I.Label lbl)) = (Right . A.LabelReference) lbl -- Convert a label to a label reference
    convertData (I.IntegerValue int) = (Right . A.IntegerValue) int -- Convert a 
    convertData (I.Ref (I.StackPointerOffset off)) = (Right . A.RelativePointer)(A.SP, off) -- Convert a stackpointer offset to a relative pointer with offset of stackpointer
    convertData (I.Ref (I.Absolute ptr)) = (Right . A.Pointer) ptr -- Convert a absolute address to a pointer
    convertData d = (trace . show) d ((Right . A.Pointer) 0) -- Others, convert to a pointer to address 0

    -- | Implementation of compilation step suitable for foldr
    compileImpl :: I.Instruction -> ([Either A.Instruction String] , HM.HashMap String A.Data) -> ([Either A.Instruction String], HM.HashMap String A.Data)

    -- | Converts a label to a label
    compileImpl  (I.Lbl lbl) (rhs, c) = (Right lbl : rhs, c)

    -- | Converts a increment to an ADD 1 to a certain register
    compileImpl  (I.Increment (I.R r)) (rhs, c) = (Left (A.ADD(armR, armR, Right (A.IntegerValue 1))) : rhs, c)
        where armR = convertRegister r

    -- | Converts a increment to an SUB 1 to a certain register
    compileImpl  (I.Decrement (I.R r)) (rhs, c) = (Left (A.SUB(armR, armR, Right (A.IntegerValue 1))) : rhs, c)
        where armR = convertRegister r
    -- | Converts a compare instruction with two registers to a CMP
    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=I.Ref (I.R r)}  (rhs, c) = (Left (A.CMP(armRL, Left armRR)) : rhs, c)
        where
            armRL = convertRegister l
            armRR = convertRegister r


    -- | Converts a compare instruction with one register and one interger value
    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=I.IntegerValue v}  (rhs, c)
        | v < 255 = (Left (A.CMP(armRL, (Right . A.IntegerValue) v)) : rhs, c)
        | otherwise = (rhs, c)
        where
            armRL = convertRegister l

    -- | Converts
    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=I.BooleanValue  v}  (rhs, c)  = (Left (A.CMP(armRL, (Right . A.IntegerValue) (if v then 1 else 0))) : rhs, c)
        where
            armRL = convertRegister l
    -- | Converts a sub to sub.
    compileImpl I.Sub{I.store=I.R store, I.lhs=I.Ref (I.R lhs), I.rhs=I.Ref (I.R r)} (rhs, c) = ((Left . A.SUB)  (convertRegister store,  convertRegister lhs, (Left . convertRegister) r): rhs , c)
    -- | Converts a goto statement to a unconditional branch to a given label
    compileImpl  (I.Goto (I.Label lbl)) (rhs, c) = (Left (A.B lbl) : rhs, c)
    -- | Routine init, push LR to stack
    compileImpl (I.RoutineInit args) (rhs, c) = ((Left . A.PUSH) [A.LR] : rhs, c)
    -- | Return none, POP PC from stack.
    compileImpl I.RetNone (rhs, c) = ((Left . A.POP) [A.PC] : rhs, c)

    -- | Convert function call to given instructions, it pushes R0-R2, then moves the arguments to the appropriate register, BL to given label, moves R0 to appropriate regsiter and pops R0-R2 from stack.
    compileImpl I.Call{I.storeReturn=Nothing, I.routine=I.Label rt, I.args=[]} (rhs, c) = ((Left . A.PUSH) [A.R0, A.R1, A.R2] : (Left . A.BL) rt : (Left . A.POP) [A.R0, A.R1, A.R2] : rhs, c)
    -- | Compile branch conditional to the right B-type.
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
    -- | Loads variable to register, depending on size of variable loads with intermediate value or loads from memory. Bug: Loads from memory not working.
    compileImpl I.PushVar{I.store=I.R reg, I.source=I.IntegerValue i} (rhs, c)
        | i < 256 = ((Left . A.MOV) (convertRegister reg, (Right . A.IntegerValue) i) : rhs, c)
        | otherwise = ((Left . A.LDR) (convertRegister reg, (Right . A.LabelReference ) valueLbl) : rhs, HM.insert valueLbl (A.IntegerValue i) c)
        where valueLbl = "VAL_"  ++ show i

    -- | Loads boolean value to register.    
    compileImpl I.PushVar{I.store=I.R reg, I.source=I.BooleanValue b} (rhs, c) =
        ((Left . A.MOV) (convertRegister reg, (Right . A.IntegerValue) (if b then 1 else 0)) : rhs, c)
    -- | Moves register to return to R0
    compileImpl (I.Ret(I.Ref (I.R (I.Register reg)))) (rhs, c)
        | reg < 9 = ((Left . A.MOV) (A.R0, (Left . convertRegister) (I.Register reg)) : rhs, c)
    -- | Moves 1, when true to R0 or 0 when false to R0.
    compileImpl (I.Ret(I.BooleanValue b)) (rhs, c)
        = ((Left . A.MOV) (A.R0, (Right . A.IntegerValue) (if b then 1 else 0)) : (Left . A.POP) [A.PC] : rhs, c)

    -- | Moves given value to R0. Does not work on values greather than 255.
    compileImpl (I.Ret(I.IntegerValue v)) (rhs, c)
        = ((Left . A.MOV) (A.R0, (Right . A.IntegerValue) v) : rhs, c)

    -- | Implements Load from stack
    compileImpl (I.StackToR regs) (rhs, c) =  ((Left . A.POP) ((map convertRegister . filter (\case (I.Register r) -> r < 4; _->False )) regs) : rhs, c)
    -- | Implements Store to stack.
    compileImpl (I.RToStack  regs) (rhs, c) = ((Left . A.PUSH) ((map convertRegister . filter (\case (I.Register r) -> r < 4; _->False )) regs) : rhs, c)

    -- | Implements function call with direct return (so the value does not have to move from R0 to another register.)
    compileImpl I.Call{I.storeReturn=Just I.ReturnValue,I.routine=I.Label rt , I.args=p} (rhs, c)
        = ( argFill ++ [(Left . A.BL) rt, (Left . A.POP)[A.PC] ] ++ rhs, c)
        where
            argFill = case p of
                [_] -> []
                (x: xs) -> map Left (zipWith (curry mapArgMoveInstruction) (map (convertRegister . I.Register) [1..]) xs)
                [] -> []

    -- | Implements function call with return value stored to a given register, so the R0 should be moved to another register before pop.
    compileImpl I.Call{I.storeReturn=Just (I.R retTarget),I.routine=I.Label rt , I.args=p} (rhs, c)
        = ( preCall ++ ((Left . A.BL) rt : postCall) ++ rhs, c)
        where
            argFill = map Left (zipWith (curry mapArgMoveInstruction) (map (convertRegister . I.Register) [0..]) p)
            retReg = convertRegister retTarget
            preCall = argFill
            postCall = (Left . A.MOV) (convertRegister retTarget, Left A.R0) : rhs


    -- | Fail safe. Shows trace to console. (Not functional, but it works very great.)
    compileImpl lhs (rhs, c) = trace ("Failed -> " ++ show lhs )(rhs, c)

    -- | 
    mapArgMoveInstruction :: (A.ArmRegister, I.Data) -> A.Instruction
    mapArgMoveInstruction (reg, I.Ref(I.R r)) = A.MOV (reg, (Left . convertRegister) r)





    -- | Implements compile instruction. Generates a Arm Document containing exported functions, read only variables, read-write variables and instructions.
    compile :: I.ILDocument -> A.ArmDocument
    compile I.ILDocument{I.instructions=(I.InstructionList lst), I.exports=e } = A.ArmDocument {
        A.exports = e,
        A.instructions = (A.ArmInstructions . fst . foldr compileImpl ([], HM.empty)) lst
    }

    









