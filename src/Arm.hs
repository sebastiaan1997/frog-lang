module Arm(ArmRegister(..), Instruction(..), ArmInstructions(..), Data(..), registerIndex) where


    -- | Datatypes for ARM processor
    data Data = IntegerValue Int -- Contains signed integer value
        | UnsignedIntegerValue Int -- Contains unsigned integer value
        | FloatValue Float -- Contains a float value, not implemented by ARM-Cortex M0, so not implemented.
        | StringValue String -- Contains a characterstring, not implemented
        | LabelReference String -- Contains a reference for a label
        | Pointer Int -- Contains a absolute adress
        | RelativePointer (ArmRegister, Int) -- Contains a relative adress to a register.
        deriving ( Eq ) -- Implements equal operator.

    instance Show Data where
        show (IntegerValue val) = '#' : show val 
        show (UnsignedIntegerValue val) = '#' : show val
        show (LabelReference val) = '.' : show val



    -- | Docs for ARM available at https://developer.arm.com/documentation/dui0497/a/the-cortex-m0-instruction-set/instruction-set-summary?lang=en
    data ArmRegister = R0 -- Register 0findEmptyGPRegister ctx = Nothing
        | R1  -- Register 1
        | R2  -- Register 2
        | R3  -- Register 3
        | R4  -- Register 4
        | R5  -- Register 5
        | R6  -- Register 6
        | R7  -- Register 7
        | R8  -- Register 8
        | R9  -- Register 9
        | R10 -- Register 10
        | R11 -- Register 11
        | R12 -- Register 12
        | SP  -- Stack Pointer register 
        | LR  -- Link Register 
        | PC  -- Program Counter register
        deriving ( Eq, Show )

    -- | Converts a ARM Register to a register index as defined by ARM.
    registerIndex :: ArmRegister -> Int
    registerIndex R0 = 0
    registerIndex R1 = 1
    registerIndex R2 = 2
    registerIndex R3 = 3
    registerIndex R4 = 4
    registerIndex R5 = 5
    registerIndex R6 = 6
    registerIndex R7 = 7
    registerIndex R8 = 8
    registerIndex R9 = 9
    registerIndex R10 = 10
    registerIndex R11 = 11
    registerIndex R12 = 12
    registerIndex SP = 12
    registerIndex LR = 13
    registerIndex PC = 14

    -- | ARM Instruction set definition
    data Instruction = Nop
        | ADCS  (ArmRegister, ArmRegister, ArmRegister) -- Add with carry, signed
        | ADC   (ArmRegister, ArmRegister, ArmRegister) -- Add with carry, unsigned
        | ADD   (ArmRegister, ArmRegister, Either ArmRegister Data)     -- Add signed
        | ADDS  (ArmRegister, ArmRegister, Either ArmRegister Data)     -- Add unsigned
        | ADR   (ArmRegister, String) 
        | ANDS  (ArmRegister, ArmRegister, ArmRegister)
        | ASRS  (ArmRegister, ArmRegister, Data)
        | B      String -- Unconditional branch, goto label in string
        | BNE    String -- Conditional not equal branch
        | BEQ    String -- Conditional equal branch
        | BGT    String -- Conditional Greather Than branch
        | BLT    String -- Conditional Lesser Than Branch
        | BLE    String -- Conditional Lesser Equal Branch
        | BGE    String -- Conditional greater or equal branch
        | BICS  (ArmRegister, ArmRegister, ArmRegister)
        | BKPT
        | BL     String     -- Branch link (function call)
        | BLX    ArmRegister
        | BBX    ArmRegister
        | CMN   (ArmRegister, ArmRegister)  -- | Negative compare
        | CMP   (ArmRegister, Either ArmRegister Data)      -- | Compare
        | CPSID Int
        | CPSIE Int
        | DMB
        | DSB
        | EORS  ( ArmRegister, ArmRegister, ArmRegister ) -- Exclusive or
        | LDR   (ArmRegister, Either ArmRegister Data)
        | LDM   ( ArmRegister, [ArmRegister] ) -- | Load Multiple registers, increment after
        | MOV   ( ArmRegister, Either ArmRegister Data )
        | MRS   (ArmRegister, ArmRegister)
        | MSR   (ArmRegister, ArmRegister)
        | MULS  ( ArmRegister, ArmRegister, ArmRegister )
        | MVNS  ( ArmRegister, ArmRegister )
        | ORRS  ( ArmRegister, ArmRegister, ArmRegister )
        | PUSH  [ ArmRegister ]
        | POP   [ ArmRegister ]
        | REV   ( ArmRegister, ArmRegister )
        | REV16 ( ArmRegister, ArmRegister )
        | REVSH ( ArmRegister, ArmRegister )
        | RORS  ( ArmRegister, ArmRegister, ArmRegister ) -- Rotate right
        | RSBS  ( ArmRegister, ArmRegister, Int ) -- Reverse substract
        | SBCS  ( ArmRegister, ArmRegister, ArmRegister ) -- Substract with Carry
        | SEV   -- Send event
        | STM   ( ArmRegister, [ArmRegister] ) -- Store multiple registers, increment after
        | STR  ( ArmRegister, Either (ArmRegister, ArmRegister) (ArmRegister, Maybe Int)) -- | Store Register as word
        | STRB ( ArmRegister, Either (ArmRegister, ArmRegister) (ArmRegister, Maybe Int) ) -- | Store Register as byte
        | STRH ( ArmRegister, Either (ArmRegister, ArmRegister) (ArmRegister, Maybe Int) ) -- | Store Register as halfword
        | SUB   (ArmRegister, ArmRegister, Either ArmRegister Data) -- | Subtract
        | SVC Int -- | 	Supervisor Call
        | SXTB  (ArmRegister, ArmRegister) -- | Sign extend byte
        | SXTH (ArmRegister, ArmRegister) -- | Sign extend halfword
        | TST (ArmRegister, ArmRegister) -- | Logical AND based test 
        | UXTB (ArmRegister, ArmRegister) -- | Zero extend a byte
        | UXTH (ArmRegister, ArmRegister) -- | Zero extend a halfword
        | WFE -- | Wait For Event
        | WFI  -- | Wait For Interrupt
        deriving ( Eq )

    -- Implements representation function for all ARM instructions
    instance Show Instruction where
        show (ADCS(res, lhs, rhs)) = "ADDCS " ++ show res ++ ", " ++ show lhs ++ ", " ++ show rhs
        show (ADC(res, lhs, rhs))  = "ADDC " ++ show res ++ ", " ++ show lhs ++ ", " ++ show rhs
        show (ADD(rd, rn, Left rm))  = "ADD " ++ show rd ++ ", " ++ show rn ++ ", " ++ show rm
        show (ADD(rd, rn, Right imm))  = "ADD " ++ show rd ++ ", " ++ show rn ++ ", " ++ show imm
        show (ADDS(rd, rn, Left rm))  = "ADDS " ++ show rd ++ ", " ++ show rn ++ ", " ++ show rm
        show (ADDS(rd, rn, Right imm))  = "ADDS " ++ show rd ++ ", " ++ show rn ++ ", " ++ show imm
        
        show (ADR(res, lbl))       = "ADR " ++ show res ++ ", " ++ lbl
        show (ANDS(res, lhs, rhs))  = "ANDS " ++ show res ++ ", " ++ show lhs ++ ", " ++ show rhs
        show (ASRS(res, lhs, rhs))  = "ANDS " ++ show res ++ ", " ++ show lhs ++ ", " ++ show rhs
        show (B lbl)  = "B " ++ lbl
        show (BNE lbl) = "BNE ." ++ lbl
        show (BEQ lbl) = "BEQ ." ++ lbl
        show (BGT lbl) = "BGT ." ++ lbl
        show (BLT lbl) = "BLT ." ++ lbl
        show (BLE lbl) = "BLE ." ++ lbl
        show (BGE lbl) = "BLE ." ++ lbl
        show (BICS (res, lhs, rhs)) = "BICS " ++ show res ++ ", " ++ show lhs ++ ", " ++ show rhs
        show BKPT = ""
        show (BL target) = "BL " ++ target
        show (BLX target) = "BLX " ++ show target
        show (BBX target) = "BBX " ++ show target
        show (CMN (lhs, rhs)) = "CMN " ++ show lhs ++ ", " ++ show rhs
        show (CMP (lhs, Left rhs)) = "CMP " ++ show lhs ++ ", " ++ show rhs
        show (CMP (lhs, Right rhs)) = "CMP " ++ show lhs ++ ", " ++ show rhs
        show (CPSID id) = "CPSID " ++ show id
        show (CPSIE id) = "CPSID " ++ show id
        show DMB = "DMB"
        show DSB = "DSB"
        show (EORS  (res, lhs, rhs))     = "EORS " ++ show res ++ ", " ++ show lhs ++ ", " ++ show rhs
        show (LDR (rt, Right (LabelReference lbl))) = "LDR " ++ show rt ++ ", ." ++ lbl
        -- show (LDR (rt, Right lbl)) = "LDR " ++ show rt ++ ", ." ++ lbl
        show (LDM   (res, registers)) = "LDM " ++ show res ++ "["  ++ registerList ++ "]"
            where
                registerList = foldr (\lhs rhs -> ',' : show lhs ++ rhs) "" registers

        show (MOV (destination, Left src)) = "MOV " ++ show destination ++ ", " ++ show src
        show (MOV (destination, Right src)) = "MOV " ++ show destination ++ ", " ++ show src
        show (MRS (destionation, src)) = "MRS " ++ show destionation ++ ", " ++ show src
        show (MSR (destination, src)) = "MSR " ++ show destination ++ ", " ++ show src
        show (MULS (rd, rn, rm)) = "MULS " ++ show rd ++ ", " ++ show rn ++ ", " ++ show rm
        show (MVNS (rd, rm)) = "MVNS " ++ show rd ++ ", " ++ show rm
        show Nop = "NOP"
        show (ORRS(rd, rn, rm)) = "ORRS " ++ show rd ++ ", " ++ show rn ++ ", " ++ show rm
        show (POP regList) = "POP [" ++ tail regListRepr ++ "]"
            where
                regListRepr = foldr (\lhs rhs -> ',' : show lhs ++ rhs) "" regList
        show (PUSH regList) = "PUSH [" ++ tail regListRepr ++ "]"
            where
                regListRepr = foldr (\lhs rhs -> ',' : show lhs ++ rhs) "" regList
        show (REV (rd, rm))  = "REV " ++ show rd ++ ", " ++ show rm
        show (REV16 (rd, rm)) = "REV16 " ++ show rd ++ ", " ++ show rm
        show (REVSH (rd, rm)) = "REVSH " ++ show rd ++ ", " ++ show rm
        show (RORS (rd, rn, rs)) = "RORS " ++ show rd  ++ ", " ++ show rn ++ ", " ++ show rs
        show (RSBS (rd, rn, rs)) = "RSBS " ++ show rd  ++ ", " ++ show rn ++ ", " ++ show rs
        show (SBCS (rd, rn, rs)) = "SBCS " ++ show rd  ++ ", " ++ show rn ++ ", " ++ show rs
        show SEV = "SEV"
        show (STM (rn, regList)) = "STM " ++ show rn ++ '[' : regListRepr ++ "]"
            where
                regListRepr = foldr (\lhs rhs -> ',' : show lhs ++ rhs) "" regList
        show (STR (rt, Left (rn, rm))) = "STR " ++ show rt ++ '[' : show rn ++ ", " ++ show rm ++ "]"
        show (STR (rt, Right (rn, imm))) = "STR " ++ show rt ++ '[' : show rn ++ ", " ++ show imm ++ "]"
        show (STRB (rt, Left (rn, rm))) = "STRB " ++ show rt ++ '[' : show rn ++ ", " ++ show rm ++ "]"
        show (STRB (rt, Right (rn, imm))) = "STRB " ++ show rt ++ '[' : show rn ++ ", " ++ show imm ++ "]"
        show (STRH (rt, Left (rn, rm))) = "STRH " ++ show rt ++ '[' : show rn ++ ", " ++ show rm ++ "]"
        show (STRH (rt, Right (rn, imm))) = "STRH " ++ show rt ++ '[' : show rn ++ ", " ++ show imm ++ "]"
        show (SUB (rt,rn, Left rm)) = "SUB " ++ show rt ++ ", " ++ show rn ++ ", " ++ show rm
        show (SUB (rt,rn, Right imm)) = "SUB " ++ show rt ++ ", " ++ show rn ++ ", " ++ show imm
        show (SVC imm) = "SVC " ++ show imm
        show (SXTB (rd, rm)) = "SXTB " ++ show rd ++ ", " ++ show rm
        show (SXTH (rd, rm)) = "SXTH " ++ show rd ++ ", " ++ show rm
        show (TST (rd, rm)) = "TST " ++ show rd ++ ", " ++ show rm
        show (UXTB (rd, rm)) = "UXTB " ++ show rd ++ ", " ++ show rm  
        show (UXTH (rd, rm)) = "UXTH " ++ show rd ++ ", " ++ show rm  
        show WFE = "WFE" 
        show WFI = "WFI"

    -- | Contains ARM 
    newtype ArmInstructions = ArmInstructions [Either Instruction String]
    -- | Implements show function for ArmInstructions.
    instance Show ArmInstructions where
        show (ArmInstructions instr) = foldl (\s i -> s ++ case i of Left i -> '\t' : show i ++ "\n"; Right lbl -> '.' : lbl ++ "\n")  [] instr


