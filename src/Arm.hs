module Arm(ArmRegister(..), ArmInstruction(..)) where 


    data Data = IntegerValue Int
        | FloatValue Float
        | StringValue String
        | Pointer Int
        | RelativePointer Int
        deriving ( Eq, Show )

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


    data ArmInstruction = Nop
        | LABEL String
        | ADCS  (ArmRegister, ArmRegister, ArmRegister) -- Add with carry
        | ADD   (ArmRegister, ArmRegister, Data)     -- Add
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
        | CMN   (ArmRegister, ArmRegister)  -- Compare
        | CMP   (ArmRegister, Data)      -- Negative compare
        | CPSID Int
        | CPSIE Int
        | DMB
        | DSB
        | EORS  ( ArmRegister, ArmRegister, ArmRegister ) -- Exclusive or
        | LDM   ( ArmRegister, [ArmRegister] )
        | MOV   ( ArmRegister, Data )
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
        | STR  ( ArmRegister, ArmRegister, Maybe Int )
        | STRH
        | SUB   (ArmRegister, ArmRegister, Data)
        | SVC
        | SXTB
        | SXTH
        | TST
        | UXTB
        | UXTH
        | WFE
        | WFI
        | TODO String
        -- | PLACEHOLDER InstructionPlaceholder
        | ERROR String
        deriving ( Eq, Show )