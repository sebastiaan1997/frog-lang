module Compiler(compile, build) where
  import Debug.Trace (trace, traceShow)
  import qualified Lexer as L
  import qualified Parser as P
  import qualified Data.Map as M


  -- | Docs for ARM available at https://developer.arm.com/documentation/dui0497/a/the-cortex-m0-instruction-set/instruction-set-summary?lang=en
  data ArmRegister = R0 -- Register 0
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
    | RegisterPlaceholder String -- Placeholder for registers, used for first compilation stage. 
    | ValuePlaceholder Int -- Placeholder for values that can be places in registers, used for first compilation stage. 
    | Empty -- Empty register 
    deriving ( Eq, Show )
  

  data ArmData = Register ArmRegister | Value Integer | Address Integer | Label String | DataPlaceholder String | CharString String | NoData
    deriving (Eq, Show)

  data Variable = Stack Int | RegisterAllocated ArmRegister | Memory Int | Constant Int
    deriving ( Eq, Show )
  data ArmCompareSuffix = None | CMP_EQ | CMP_NE | CMP_HS | CMP_LO | CMP_MI | CMP_PL | CMP_VS | CMP_VC | CMP_HI | CMP_LS | CMP_GE | CMP_LT | CMP_GT | CMP_LE | CMP_AL
    deriving ( Eq, Show )


  data InstructionPlaceholder = CREATE_VAR (String, ArmData, String) 
    | SAVE_VARIABLE String
    | LOAD_VALUE_TO_REGISTER { value :: ArmData, register :: ArmRegister}
    | LOAD_VARIABLE_TO_REGISTER { variable :: String, register :: ArmRegister }
    | DUMP_REGISTER ArmRegister -- Indication that the given register will be lost, and the value will be changed to another variable that is not suitable in the given context.
    | SAVE_REGISTERS    -- Indicates that the stack that the stack of the current function wi
    | RESTORE_REGISTERS -- Indicates that the current stack
    | FUNCTION_BEGIN String -- Indicates that a function has started here.
    | FUNCTION_END   String -- Indicates that a given function has ended 
    deriving ( Eq, Show )
  -- | 
  data ArmInstruction = Nop
    | LABEL String
    | ADCS  (ArmRegister, ArmRegister, ArmRegister) -- Add with carry
    | ADD   (ArmRegister, ArmRegister, ArmData)     -- Add
    | ADR   (ArmRegister, String)
    | ANDS  (ArmRegister, ArmRegister, ArmRegister)
    | ASRS  (ArmRegister, ArmRegister, ArmData)
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
    | CMP   (ArmRegister, ArmData)      -- Negative compare
    | CPSID Int
    | CPSIE Int
    | DMB
    | DSB
    | EORS  ( ArmRegister, ArmRegister, ArmRegister ) -- Exclusive or
    | LDM   ( ArmRegister, [ArmRegister] )
    | LDR   ( ArmRegister, Either String (ArmRegister, ArmData) )
    | MOV   ( ArmRegister, ArmRegister )
    | MRS   ( ArmRegister, ArmRegister )
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
    | STR  ( ArmRegister, ArmRegister )
    | STRH
    | SUB   (ArmRegister, ArmRegister, ArmData) 
    | SVC
    | SXTB
    | SXTH
    | TST
    | UXTB
    | UXTH
    | WFE
    | WFI
    | PLACEHOLDER InstructionPlaceholder
    | ERROR String
    deriving ( Eq, Show )


  data CompilerContext = CompilerContext { labelCounter :: Int, registers :: M.Map String ArmRegister, stack :: [String] }
    deriving ( Eq, Show )
    
  compareOperation :: String -> Maybe ArmCompareSuffix
  compareOperation "==" = Just CMP_EQ
  compareOperation "!=" = Just CMP_NE
  compareOperation ">"  = Just CMP_GT
  compareOperation ">=" = Just CMP_GE
  compareOperation "<=" = Just CMP_LS
  compareOperation _ = Nothing


  toArmData :: P.FrogNode -> ArmData
  toArmData (P.NumericLiteral num)    = Value (round num)
  toArmData (P.StringLiteral  str)    = CharString str
  toArmData (P.BooleanLiteral True)   = Value 1
  toArmData (P.BooleanLiteral False)  = Value 0
  toArmData (P.Variable var)          = DataPlaceholder var
  toArmData node = trace ("Could not execute toArmData for " ++ show node) NoData


  -- | Generate instructions for conditional branches
  compileConditionalBranch :: P.FrogNode -> String -> Maybe String -> [ArmInstruction]
  compileConditionalBranch P.InfixCall{P.lhs=lhs,P.function=func, P.rhs=rhs} target Nothing
    | func == "==" = loadInstruction : [ CMP (R4, rhsValue), BEQ target ]
    | func == "!=" = loadInstruction : [ CMP (R4, rhsValue), BNE target ]
    | func == "<"  = loadInstruction : [ CMP (R4, rhsValue), BLT target ]
    | func == ">"  = loadInstruction : [ CMP (R4, rhsValue), BGT target ]
    | func == ">=" = loadInstruction : [ CMP (R4, rhsValue), BGE target ]
    | func == "<=" = loadInstruction : [ CMP (R4, rhsValue), BLE target ]
    where 
        lhsValue = toArmData lhs
        rhsValue = toArmData rhs
        loadInstruction = PLACEHOLDER LOAD_VALUE_TO_REGISTER {value=lhsValue, register=R4}
  -- generateFunctionStack :: [] -> CompilerContext
  -- |
  -- generateFunctionStack parameters = CompilerContext{    }

  compile :: P.FrogNode -> [ArmInstruction]

  compile (P.Sequence seq) = foldl (++) [] (map (compile) seq)
  compile (P.Statement stmt) = compile stmt



  compile (P.VarDeclaration {P.name=name, P.assignment=P.Token(L.DigitLiteral value)}) = [ PLACEHOLDER(CREATE_VAR(name, Value (round value), ""))]
  compile (P.VarDeclaration {P.name=name, P.assignment=P.NumericLiteral value}) = [ PLACEHOLDER(CREATE_VAR(name, Value (round value), "int"))]
  compile (P.VarDeclaration {P.name=name, P.assignment=P.BooleanLiteral value}) = [ PLACEHOLDER(CREATE_VAR(name, Value (if not(value) then 0 else 1), "bool"))]
    -- where asm = compile (P.Sequence assignment)



  compile (P.FunctionDefinition{P.name=name, P.params=parameters, P.body=body}) = LABEL ("func" ++ name) : compile (P.Sequence body)
  compile (P.RoutineDefinition {P.name=name, P.params=parameters, P.body=body}) = LABEL ("func" ++ name) : compile (P.Sequence body)
  
  -- | Optimize always true if statement, do not emit branches
  compile (P.IfStatement{P.condition=P.BooleanLiteral True, P.body=b}) = [ERROR "Compile for trueval not implemented"]
  
  -- | Optimize always false if statement, do not emit code at all
  compile (P.IfStatement{P.condition=P.BooleanLiteral False, P.next=nothing})  = []
  
  -- | No optimizations possible
  compile (P.IfStatement {P.condition=P.InfixCall{P.lhs=P.Variable(lhs), P.function=operator, P.rhs=P.Variable(rhs)}, P.body=body, P.next=Nothing}) 
    | operator == "==" = compareInstruction ++ [ BNE ".if_jump_1" ] ++ compile (P.Sequence  body) ++ [LABEL ".if_jump_1"]
    | operator == "!=" = compareInstruction ++ [ BEQ ".if_jump_1" ] ++ compile (P.Sequence  body) ++ [LABEL ".if_jump_1"]
    | operator == ">"  = compareInstruction ++ [ BLE ".if_jump_1" ] ++ compile (P.Sequence  body) ++ [LABEL ".if_jump_1"]
    | operator == "<"  = compareInstruction ++ [ BGE ".if_jump_1" ] ++ compile (P.Sequence  body) ++ [LABEL ".if_jump_1"]
    where compareInstruction = [CMP(RegisterPlaceholder lhs, DataPlaceholder rhs)]

  


  compile (P.IfStatement {P.condition=P.Variable{P.name=name}, P.body=body, P.next=Nothing}) =  
    [CMP (RegisterPlaceholder name, Value 0)] ++ bodyInstructions ++ [BEQ ".if_jump_1" ]
    where bodyInstructions = compile (P.Sequence body)

  
  -- | 
  compile (P.IfStatement {P.condition=P.InfixCall{P.lhs=P.Variable lhs, P.function=operator, P.rhs=P.Variable rhs}, P.body=body, P.next=next}) = 
    [ CMP(RegisterPlaceholder lhs, DataPlaceholder rhs) ]
  -- | 
  compile (P.InfixCall {P.lhs=P.Token(L.DigitLiteral(lhs)), P.function=func, P.rhs=P.Token(L.DigitLiteral(rhs))}) = [BL func]

  -- | 
  compile (P.InfixCall {P.lhs=P.Variable(lhs), P.function=func, P.rhs=P.Variable(rhs)}) 
    | func == "+"  = [ ADD  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | func == "-"  = [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | func == "*"  = [ MULS (R4, RegisterPlaceholder lhs, RegisterPlaceholder rhs ) ]
    | func == "/"  = [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | func == "<"  = [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | func == "==" = [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | func == ">"  = [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | func == "<=" = [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | func == ">=" = [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ]
    | otherwise    = [ MOV  (R4, RegisterPlaceholder lhs), MOV(R1, RegisterPlaceholder rhs), BL func ] -- Otherwise call function with name.

  
  
  -- | 
  compile (P.SelfAssigningInfixCall {P.lhs=P.Variable(lhs), P.function=func, P.rhs=P.Variable(rhs)})
    | func == "+=" = [ ADD (RegisterPlaceholder lhs, RegisterPlaceholder lhs, DataPlaceholder rhs) ]
    | func == "-=" = [ SUB (RegisterPlaceholder lhs, RegisterPlaceholder lhs, DataPlaceholder rhs) ]
    | func == "*=" = [ MULS(RegisterPlaceholder lhs, RegisterPlaceholder lhs, RegisterPlaceholder rhs) ]
    | func == "/=" = [ SUB (RegisterPlaceholder lhs, RegisterPlaceholder lhs, DataPlaceholder rhs) ]
    | func == "/=" = [ SUB (RegisterPlaceholder lhs, RegisterPlaceholder lhs, DataPlaceholder rhs) ]
    | otherwise   =  [ BL func ]


  compile (P.SelfAssigningInfixCall {P.lhs=P.Variable(lhs), P.function=func, P.rhs=P.Nop})
    | func == "++" = [ADD (RegisterPlaceholder lhs, RegisterPlaceholder lhs, Value 1)]
    | func == "--" = [SUB (RegisterPlaceholder lhs, RegisterPlaceholder lhs, Value 1)]
    -- | otherwise    = [ LDR (R0, Right( DataPlaceholder lhs)), LDR (R1, Right( DataPlaceholder rhs)),  BL func ]
  
  compile (P.WhileStatement {P.condition=condition, P.body=body}) = asmCondition ++ asmBody
    where 
      asmCondition = compileConditionalBranch condition ".whileStart-1" Nothing
      asmBody = compile (P.Sequence body)
  compile (P.FunctionReturn node) = compile node ++ [MOV (PC, LR)]
  compile (P.FunctionCall{P.target=target, P.params=params}) = [ BL target ]
    -- where parameterInstructions

  -- compile (P.VarDeclaration{name})
  compile node = trace ("Could not process node " ++ show node) [ERROR ("Could not process node " ++ show node)]

  initialCompilerContext :: CompilerContext
  initialCompilerContext = CompilerContext { labelCounter=0, registers=M.empty, stack=[] }

  build :: P.FrogNode -> [ArmInstruction]
  build = compile
