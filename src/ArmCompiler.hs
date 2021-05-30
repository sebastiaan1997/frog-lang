module ArmCompiler(compile) where
    import qualified Il as I
    import qualified Arm as A
    import qualified Data.HashSet as S


    -- Call { storeReturn :: Maybe Reference, routine :: Reference, args :: [ Data ] }
    --     | Goto Reference
    --     | CmpStmtNext   { lhs :: Data, rhs :: Data, conditionType :: BranchType }
    --     | Cmp           { lhs :: Data, rhs :: Data } -- Compare two values for next instruction
    --     | CmpStore      { store :: Reference, lhs :: Data, rhs :: Data }
    --     | Add           { store :: Reference, lhs :: Data, rhs :: Data }
    --     | Sub           { store :: Reference, lhs :: Data, rhs :: Data }
    --     | Mul           { store :: Reference, lhs :: Data, rhs :: Data }
    --     | Div           { store :: Reference, lhs :: Data, rhs :: Data }
    --     | B             { conditionType :: BranchType, lhs :: Data, rhs :: Data, target :: Reference }
    --     | BC            { conditionType :: BranchType, target :: Reference }
    --     | Increment     Reference -- Increment by 1
    --     | Decrement     Reference -- Decrement by 1
    --     | Jump Reference
    --     | Load Reference
    --     | Store Reference
    --     | LoadToReg (Register, Reference)
    --     | StoreFromReg (Register, Reference)
    --     | Create Reference
    --     | Push Data
    --     | PushVar { store :: Reference, source :: Data }
    --     | Pop
    --     | PopVar [Reference]
    --     | Remove Reference
    --     | Sum [Data]
    --     | Set { store :: Reference, source :: Data }
    --     | Lbl String
    --     | Ret Data
    --     | RetNone

        -- instance Show Reference where
        -- show (R r) = show  r
        -- show (Label lbl) = '.' : lbl
        -- show (StackPointerOffset o) = "[sp, #" ++ show o ++ "]"
        -- show StackframeOffset{context=ctx, offset=o} = '"' : ctx ++ "\"->[ sp, " ++ show o ++ "]"
        -- show (Absolute addr) =  "0x" ++ showHex addr ""
        -- show (Variable var)  = '`' : var ++ "`"
        -- show (Offset (ref, o)) = '[' : show ref ++ ", " ++ show o ++ "]"
        -- show Prev = "prev"
        -- show Next = "next"
        -- show Parent = "parent"


    convertRegister :: I.Register -> A.ArmRegister
    convertRegister (I.Register r) = [A.R0, A.R1, A.R2, A.R3, A.R4, A.R5, A.R6, A.R7, A.R8, A.R9, A.R10, A.R11, A.R12] !! r

    convertData :: I.Data -> A.Data
    convertData (I.Ref (I.Label lbl)) = A.LabelReference lbl
    convertData (I.IntegerValue int) = A.IntegerValue int
    convertData (I.Ref (I.StackPointerOffset off)) = A.RelativePointer(A.SP, off)
    convertData (I.Ref (I.Absolute ptr)) = A.Pointer ptr

    compileImpl :: I.Instruction -> ([A.Instruction] , S.HashSet A.Data) -> ([A.Instruction], S.HashSet A.Data)
    compileImpl  (I.Lbl lbl) rhs = A.LABEL lbl : rhs

    compileImpl  (I.Increment (I.R r)) (rhs, c) = (A.ADD(armR, armR, A.IntegerValue 1) : rhs, c)
        where armR = convertRegister r

    compileImpl  (I.Decrement (I.R r)) rhs = A.SUB(armR, armR, A.IntegerValue 1) : rhs
        where armR = convertRegister r

    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=I.Ref (I.R r)}  rhs = A.CMP(armRL, Left armRR) : rhs
        where 
            armRL = convertRegister l
            armRR = convertRegister r

    compileImpl  I.Cmp{I.lhs=I.Ref (I.R l), I.rhs=r}  rhs = A.CMP(armRL, (Right . convertData) r) : rhs
        where 
            armRL = convertRegister l
            
    -- compileImpl  (I.Add{I.store=I.R r}) rhs = A.ADD()

    compileImpl  (I.Goto (I.Label lbl)) rhs = A.B lbl : rhs

    compileImpl lhs rhs = []

    compile :: I.InstructionList -> A.ArmDocument
    compile inpt = A.ArmDocument []
    




    
