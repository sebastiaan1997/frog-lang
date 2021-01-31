module Compiler(compile, build, ArmInstruction(..), ArmRegister(..), ArmData(..)) where
  import Debug.Trace (trace, traceShow)
  import Data.List(elemIndex, find)
  import Data.Maybe(isJust)


  import qualified Lexer as L
  import qualified Parser as P
  import qualified Data.Map as M
  import qualified Data.Set as S

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
    | RegisterPlaceholder String -- Placeholder for registers, used for first compilation stage. 
    | ValuePlaceholder Int -- Placeholder for values that can be places in registers, used for first compilation stage. 
    | Empty -- Empty register 
    | InvalidRegister
    deriving ( Eq, Show )
  gpRegisters :: [ArmRegister]
  gpRegisters = [R0, R1, R2, R3, R4, R5, R6, R7, R8]


  isGpRegister :: ArmRegister -> Bool
  isGpRegister reg = elem reg gpRegisters

  -- | Placeholder for data
  data ArmData = Register ArmRegister | Value Integer | Address Integer | Label String | DataPlaceholder String | CharString String | NoData
    deriving (Eq)
  
  instance Show ArmData where
    show (Register reg) = 
      case reg of
        R0 -> "R0"
        R1 -> "R1"
        R2 -> "R2"
        R3 -> "R3"
        R4 -> "R4"
        R5 -> "R5"
        R6 -> "R6"
        R7 -> "R7"
        R8 -> "R8"
        R9 -> "R9"
        R10 -> "R10"
        R11 -> "R11"
        R12 -> "R12"
        SP -> "SP"
        LR -> "LR"
        PC -> "PC"

    show (Value val) = '#' : show val
    show (Address val) = '#' : show val
    show (Label lbl) = lbl
    show (DataPlaceholder lbl) = "DataPlaceholder ( " ++ lbl ++ " )"
    show (CharString lbl) = '"' : lbl ++ "\""


  data Variable = Stack Int | RegisterAllocated ArmRegister | Memory Int | Constant Int
    deriving ( Eq, Show )
  data ArmCompareSuffix = None | CMP_EQ | CMP_NE | CMP_HS | CMP_LO | CMP_MI | CMP_PL | CMP_VS | CMP_VC | CMP_HI | CMP_LS | CMP_GE | CMP_LT | CMP_GT | CMP_LE | CMP_AL
    deriving ( Eq, Show )


  data InstructionPlaceholder = CREATE_VAR (String, ArmData, String)
    | SAVE_VARIABLE String
    | LOAD_VALUE_TO_REGISTER { value :: ArmData, register :: ArmRegister}
    | LOAD_VALUE { value :: ArmData }
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
    | MOV   ( ArmRegister, ArmData )
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
    | SUB   (ArmRegister, ArmRegister, ArmData)
    | SVC
    | SXTB
    | SXTH
    | TST
    | UXTB
    | UXTH
    | WFE
    | WFI
    | TODO String
    | PLACEHOLDER InstructionPlaceholder
    | ERROR String
    deriving ( Eq, Show )


  


  data CompilerContext = CompilerContext { labelCounter :: Int, registers :: M.Map String ArmRegister, stack :: [String], outOfMemoryVariables :: M.Map String ArmData, strings :: [String], variables :: M.Map String ArmData, constants :: M.Map String ArmData, lastUse :: M.Map String Int, lockedVariables :: S.Set String }
    deriving ( Eq, Show )
  



  -- | Create empty CompilerContext data structure
  initialCompilerContext :: CompilerContext
  initialCompilerContext = CompilerContext {labelCounter=0, registers=M.empty, stack=[], outOfMemoryVariables=M.empty, strings=[], variables=M.empty , constants=M.empty, lastUse=M.empty, lockedVariables=S.empty}

  isVariableOutOfMemory :: String -> CompilerContext -> Bool
  isVariableOutOfMemory name ctx = M.member name (outOfMemoryVariables ctx)

  registerVariable :: String -> ArmData -> CompilerContext -> CompilerContext
  registerVariable name value ctx = ctx{outOfMemoryVariables=M.insert name value (outOfMemoryVariables ctx)}

  pushStack :: String -> CompilerContext -> Maybe (CompilerContext, [ArmInstruction])
  pushStack name ctx
    | variableInRegister name ctx = reg >>= (\r -> Just (ctx{stack=name : stack ctx}, [PUSH [r]] ) )
    | otherwise = Nothing
    where
      reg = lookupRegisterByVariable name ctx



  popStack :: CompilerContext -> Maybe (String, CompilerContext, [ArmInstruction])
  popStack CompilerContext{stack=[]} = Nothing
  popStack ctx = Just(head s, ctx{stack=tail s}, []) -- TODO Add pop instructions
    where s = stack ctx
  -- popStack name ctx = ctx{stack=name : (stack ctx)}


  -- | Marks a variable that cannot be moved out of a register, so the underlying register may not be unloaded or moved.
  lockVariable :: String -> CompilerContext -> Maybe CompilerContext
  lockVariable name ctx
    | S.member name (lockedVariables ctx) =  Nothing
    | otherwise = Just ctx{lockedVariables=S.insert name (lockedVariables ctx)}
  -- | Unmarks a variable that cannot be moved out of a register, so the underlying register may be unloaded or moved again.
  unlockVariable :: String -> CompilerContext -> Maybe  CompilerContext
  unlockVariable name ctx
    | S.notMember name (lockedVariables ctx) = Nothing
    | otherwise = Just ctx{lockedVariables= S.delete name (lockedVariables ctx)}

  -- | Checks wether a variable is locked. When locked the variable cannot be moved or unloaded.
  isLocked :: String -> CompilerContext -> Bool
  isLocked name ctx = S.member name (lockedVariables ctx)

  isUnlocked :: String -> CompilerContext -> Bool
  isUnlocked name ctx = not (isLocked name ctx)

  isRegisterContainingValue :: ArmRegister -> CompilerContext -> Bool
  isRegisterContainingValue register ctx = register `elem` registers ctx


  isRegisterFree :: ArmRegister -> CompilerContext -> Bool
  isRegisterFree register ctx = not(isRegisterContainingValue register ctx)

  lookupConstant :: String -> CompilerContext -> Maybe ArmData
  lookupConstant name ctx = M.lookup name (constants ctx)

  -- | Returns wether a variable is a constant
  isConstant :: String -> CompilerContext -> Bool
  isConstant name ctx = isJust (lookupConstant name ctx)


  -- | If available, gets the register where a variable is loaded  
  lookupRegisterByVariable :: String -> CompilerContext -> Maybe ArmRegister
  lookupRegisterByVariable name ctx = M.lookup name (registers ctx)

  -- | Checks wether a variable is loaded into a register
  variableInRegister :: String -> CompilerContext -> Bool
  variableInRegister name ctx  = isJust (lookupRegisterByVariable name ctx)

  lookupVariableOnStack :: String -> CompilerContext -> Maybe Int
  lookupVariableOnStack name ctx = elemIndex name (stack ctx)

  isVariableOnStack :: String -> CompilerContext -> Bool
  isVariableOnStack name ctx = isJust (lookupVariableOnStack name ctx)

  createStringLabel :: Int -> String
  createStringLabel index = ".s_" ++ show index
  -- | Store string to a given special storage space.
  storeString :: String -> CompilerContext -> (String, CompilerContext)
  storeString newStr ctx = case (elemIndex newStr (strings ctx))  of
    Just i  -> (createStringLabel i, ctx)
    Nothing -> (createStringLabel (length (strings ctx)), ctx{strings=(strings ctx) ++ [newStr]})

  removeVariable :: String -> CompilerContext -> Maybe CompilerContext
  -- | Remove a given variable from the context
  removeVariable name ctx
    | isLocked name ctx = Nothing -- If the variable is locked, it is not allowed to delete the variable
    | otherwise = Just ctx{registers=regs, stack=s, constants=c, lastUse=lu} -- If the variable is unlocked, delete 
    where
      regs  = M.delete name (registers ctx) -- Remove from registers
      s = filter (/=name) (stack ctx)       -- Remove from stack
      c = M.delete name (constants ctx)     -- Remove from constants
      lu = M.delete name (lastUse ctx)      -- Remove from the last use index




  findStringLabel :: String -> CompilerContext -> Maybe String
  findStringLabel _ CompilerContext{strings=[]} = Nothing
  findStringLabel "" _ = Nothing
  findStringLabel contents CompilerContext{strings=ctxStrings} = case index of
      Just i -> Just (createStringLabel i)
      Nothing -> Nothing
    where index =  elemIndex contents ctxStrings


  -- createVariable :: P.FrogNode -> CompilerContext -> (CompilerContext, [ArmInstruction])
  findEmptyGPRegister :: CompilerContext -> Maybe ArmRegister
  findEmptyGPRegister ctx
    | null gpRegisters = Just R4
    | otherwise = traceShow result result
    where
      result = find (`notElem` loadedGpRegisters) gpRegisters
      loadedRegisters = M.elems (registers ctx)
      loadedGpRegisters = filter isGpRegister loadedRegisters



  isGPRegisterFree :: CompilerContext -> Bool
  isGPRegisterFree ctx = any (`notElem` loadedGpRegisters) gpRegisters
    where
      loadedRegisters = M.elems (registers ctx)
      loadedGpRegisters = filter isGpRegister loadedRegisters

  storeVariableToStack :: String -> CompilerContext -> Maybe (Int, CompilerContext, [ArmInstruction])
  storeVariableToStack name ctx
    | variableInRegister name ctx && isVariableOnStack name ctx = variableRegister >>=  (\r -> stackOffset >>=(\o -> Just (o, ctx{registers=M.delete name (registers ctx)}, [STR(r, SP, stackOffset)])))
    | variableInRegister name ctx = pushInstruction >>= (\(newCtx, inst) ->  Just(0, newCtx{registers=M.delete name (registers newCtx)},  inst))
    | otherwise = Nothing
    where
      variableRegister = lookupRegisterByVariable name ctx
      stackOffset = lookupVariableOnStack name ctx
      pushInstruction = pushStack name ctx

  loadVariableFromStack :: String -> CompilerContext -> Maybe (ArmRegister, CompilerContext, [ArmInstruction])
  loadVariableFromStack name ctx
    | isVariableOnStack name ctx && isGPRegisterFree ctx = Nothing
    | isVariableOnStack name ctx = Nothing
    | otherwise = Nothing



  acquireGPRegister :: CompilerContext  -> Maybe (ArmRegister, [ArmInstruction])
  acquireGPRegister ctx
    | isJust emptyRegister = emptyRegister >>= (\x -> Just (x, [ ]))
    | otherwise = unlockedRegister >>= (\(_,x) -> Just (x, [ ]))
    where
      emptyRegister = findEmptyGPRegister ctx
      unlockedRegister = find (\(name, _) -> isUnlocked name ctx ) (M.assocs (registers ctx))

  loadValueIntoMemory :: String -> CompilerContext -> Maybe (ArmRegister, CompilerContext, [ArmInstruction])
  loadValueIntoMemory name ctx
    | M.notMember name outOfMemoryVar = trace "loadValueIntoMemory - not found"  Nothing
    | isGPRegisterFree ctx = trace ("loadValueIntoMemory -  isGPRegisterFree " ++ (show freeResult)) freeResult
    | otherwise = trace "loadValueIntoMemory - otherwise" Nothing
    where
      outOfMemoryVar = outOfMemoryVariables ctx
      regs = registers ctx
      val = M.lookup name outOfMemoryVar
      freeRegister = findEmptyGPRegister ctx
      freeResult = freeRegister >>= (\r -> val >>= (\v -> Just (r, ctx{registers=M.insert name r regs}, [MOV(r, v)])))




  ensureVariableLoaded :: String -> CompilerContext -> Maybe (ArmRegister, CompilerContext, [ArmInstruction])
  ensureVariableLoaded name ctx
    | variableInRegister name ctx     = lookupRegisterByVariable name ctx >>= (\reg -> Just (reg, ctx, []))
    | isVariableOnStack  name ctx     = loadVariableFromStack name ctx
    | isVariableOutOfMemory name ctx  = loadValueIntoMemory name ctx


  variableAtRegister :: ArmRegister -> CompilerContext -> Maybe String
  variableAtRegister reg CompilerContext{registers=r}
    | M.null r  = Nothing
    | otherwise = find (\(k, v) -> v == reg)(M.assocs r) >>= (\(k,_) -> Just k)


  -- | Just use the move instruction, and don't bother what is in the target register. It may destroy data.
  moveOverwriteRegister :: ArmRegister -> ArmRegister -> CompilerContext -> Maybe (CompilerContext, [ArmInstruction])
  moveOverwriteRegister regTarget regSource ctx
    | regSource == regTarget = Just (ctx, []) -- Don't emit code when overwriting itself
    | otherwise = newCtx >>= (\nCtx -> Just(nCtx, [MOV (regTarget, Register regSource)]))
    where
      srcVar = variableAtRegister regSource ctx
      newCtx = srcVar >>= (\v -> Just (ctx{registers=M.update (\_ -> Just regTarget) v (registers ctx)}))
  -- | Moves the register to another position. The value at the target will be stored in RAM.
  moveReplaceStoreRegister :: ArmRegister -> ArmRegister -> CompilerContext -> Maybe (CompilerContext, [ArmInstruction])
  moveReplaceStoreRegister regTarget regSource ctx
    | regTarget == regSource = Just (ctx, [])
    | isRegisterFree regTarget ctx = moveOverwriteRegister regTarget regSource ctx -- If the register is free, then just move register
    | otherwise = stored >>=(\(_,_,asm) -> moved >>=(\(c,mAsm) -> Just(c, asm ++ mAsm))) -- If the target register is not free, store the register to RAM.
    where
      srcVar = variableAtRegister regSource ctx
      tVar   = variableAtRegister regTarget ctx
      stored = tVar >>= (`storeVariableToStack` ctx)
      moved  = stored >>= (\(_,c, asm) -> moveOverwriteRegister regTarget regSource c)
      -- var = storeVariableToStack 

  ensureVariableLoadedIntoRegister :: String -> ArmInstruction -> CompilerContext -> Maybe (ArmRegister, CompilerContext, [ArmInstruction])
  ensureVariableLoadedIntoRegister name reg ctx
    | variableInRegister name ctx = lookupRegisterByVariable name ctx >>= (\reg -> Just (reg, ctx, []))

  compareOperation :: String -> Maybe ArmCompareSuffix
  compareOperation "==" = Just CMP_EQ
  compareOperation "!=" = Just CMP_NE
  compareOperation ">"  = Just CMP_GT
  compareOperation ">=" = Just CMP_GE
  compareOperation "<=" = Just CMP_LS
  compareOperation _ = Nothing


  toArmData :: P.FrogNode -> CompilerContext -> Maybe (ArmData, CompilerContext, [ArmInstruction])
  toArmData (P.NumericLiteral num)  ctx   = Just (Value (round num), ctx, [])
  toArmData (P.StringLiteral  str)  ctx   = Just (Label lbl, newCtx, [])
    where (lbl, newCtx) = storeString str ctx
  toArmData (P.BooleanLiteral True) ctx  = Just (Value 1, ctx, [])
  toArmData (P.BooleanLiteral False) ctx = Just (Value 0, ctx, [])
  toArmData (P.Variable var) ctx  = instructions >>= (\(reg, nCtx, asm) -> Just (Register reg, nCtx, asm))
    where instructions = ensureVariableLoaded var ctx
  toArmData node _ = trace ("Could not execute toArmData for " ++ show node) Nothing


  -- | Generate instructions for conditional branches(
  compileConditionalBranch :: P.FrogNode -> String -> Maybe String -> CompilerContext -> (CompilerContext, [ArmInstruction])
  compileConditionalBranch P.InfixCall{P.lhs=lhs,P.function=func, P.rhs=rhs} target Nothing ctx
    | func == "==" = (ctx, asm ++ [ CMP (lhsRegister, rhsRegister), BEQ target ])
    | func == "!=" = (ctx, asm ++ [ CMP (lhsRegister, rhsRegister), BNE target ])
    | func == "<"  = (ctx, asm ++ [ CMP (lhsRegister, rhsRegister), BLT target ])
    | func == ">"  = (ctx, asm ++ [ CMP (lhsRegister, rhsRegister), BGT target ])
    | func == ">=" = (ctx, asm ++ [ CMP (lhsRegister, rhsRegister), BGE target ])
    | func == "<=" = (ctx, asm ++ [ CMP (lhsRegister, rhsRegister), BLE target ])
    | otherwise = (ctx, [])
    where
        lhsValue = toArmData lhs ctx
        rhsValue = lhsValue >>= (\(lVal, lCtx, lAsm) -> (toArmData rhs lCtx) >>= (\(rVal, rCtx, rAsm) -> Just (lVal, rVal, rCtx, lAsm ++ rAsm)))
        asm = case rhsValue of
          Just (lReg, rReg, ctx, asm) -> asm
          otherwise -> case lhsValue of Just (lReg, ctx, asm) -> asm; otherwise -> []
        lhsRegister = case lhsValue of Just (Register reg, _, _) -> reg; otherwise -> R4;
        rhsRegister = case rhsValue of Just (_, rVal, _,_) -> rVal; otherwise -> NoData;
        nCtx = case rhsValue of Just(_,_,rCtx,_) -> rCtx; otherwise -> case lhsValue of Just(_, lCtx, _) -> lCtx; otherwise -> ctx

  -- |
  -- generateFunctionStack parameters = CompilerContext{    }



  compile :: P.FrogNode -> CompilerContext -> ([ArmInstruction], CompilerContext)

  compile (P.Sequence seq) ctx = foldl (\(asm, ctx) func -> (asm ++ (fst (func ctx)), (snd (func ctx))) ) ([], ctx) appliedCompileFunction -- foldl \(\((lhs, ctx), rhs) -> (lhs + ) [] (map (compile) seq)
    where appliedCompileFunction = map compile seq


  compile (P.Statement stmt) ctx = compile stmt ctx -- Unwrap statement



  compile (P.VarDeclaration {P.name=name, P.assignment=a}) ctx = case newCtx of Just c -> c; otherwise -> ([ ], ctx)
    where
      converted = toArmData a ctx
      newCtx = converted >>= (\(d, ctx, asm) -> Just (asm, registerVariable name d ctx))
  -- compile (P.VarDeclaration {P.name=name, P.assignment=P.NumericLiteral value}) ctx = ([ PLACEHOLDER(CREATE_VAR(name, Value (round value), "int"))], ctx)
  -- compile (P.VarDeclaration {P.name=name, P.assignment=P.BooleanLiteral value}) ctx = ([ PLACEHOLDER(CREATE_VAR(name, Value (if not(value) then 0 else 1), "bool"))], ctx)
    -- where asm = compile (P.Sequence assignment)



  compile (P.FunctionDefinition{P.name=name, P.params=parameters, P.body=body}) ctx = (LABEL name : nextInstructions, nextCtx)
    where (nextInstructions, nextCtx) = compile (P.Sequence body) ctx


  compile (P.RoutineDefinition {P.name=name, P.params=parameters, P.body=body}) ctx = (LABEL (name) : nextInstructions, nextCtx)
    where (nextInstructions, nextCtx) = compile (P.Sequence body) ctx

  -- | Optimize always true if statement, do not emit branches
  compile (P.IfStatement{P.condition=P.BooleanLiteral True, P.body=b}) ctx = ([ERROR "Compile for trueval not implemented"], ctx)

  -- | Optimize always false if statement, do not emit code at all
  compile (P.IfStatement{P.condition=P.BooleanLiteral False, P.next=nothing}) ctx = ([], ctx)

  -- | No optimizations possible
  compile (P.IfStatement {P.condition=P.InfixCall{P.lhs=P.Variable(lhs), P.function=operator, P.rhs=P.Variable(rhs)}, P.body=body, P.next=Nothing}) ctx
    | operator == "==" = (compareInstruction ++ [ BNE ".if_jump_1" ] ++ bodyInstructions ++ [LABEL ".if_jump_1"], bodyCtx)
    | operator == "!=" = (compareInstruction ++ [ BEQ ".if_jump_1" ] ++ bodyInstructions ++ [LABEL ".if_jump_1"], bodyCtx)
    | operator == ">"  = (compareInstruction ++ [ BLE ".if_jump_1" ] ++ bodyInstructions ++ [LABEL ".if_jump_1"], bodyCtx)
    | operator == "<"  = (compareInstruction ++ [ BGE ".if_jump_1" ] ++ bodyInstructions ++ [LABEL ".if_jump_1"], bodyCtx)
    where
      lhsLoaded = ensureVariableLoaded lhs ctx
      rhsLoaded = lhsLoaded >>= (\(l,lc,la) -> ensureVariableLoaded rhs lc >>= (\(r, rc, ra) -> Just (l, r, rc, la++ra) ))
      loadAsm = case rhsLoaded of 
        Just (_, _, _, lra) -> lra; 
        _ -> case lhsLoaded of Just(_, _, la) -> la; _ -> []

      compareInstruction = loadAsm ++ [CMP(RegisterPlaceholder lhs, DataPlaceholder rhs)]
      (bodyInstructions, bodyCtx) = compile (P.Sequence  body) ctx

  compile (P.IfStatement {P.condition=P.Variable{P.name=name}, P.body=body, P.next=Nothing}) ctx =
    (conLoadAsm ++ [CMP (compReg, Value 0), BEQ ".ifJump1" ] ++ bodyInstructions ++ [LABEL ".ifJump1"], bodyCtx)
    where 
      conLoadAsmResult = ensureVariableLoaded name ctx
      compReg = case conLoadAsmResult of Just (r, _, _) -> r; _ -> InvalidRegister
      conLoadAsm = case conLoadAsmResult of Just(_,_, asm) -> asm; _ -> []
      bodyAsmResult = conLoadAsmResult >>= (\(_,c, asm) -> Just (compile (P.Sequence body) c))
      (bodyCtx, bodyInstructions) = case bodyAsmResult of Just (c, asm) -> (asm,c); _ -> (ctx, [])

  -- | 
  compile (P.IfStatement {P.condition=P.InfixCall{P.lhs=P.Variable lhs, P.function=operator, P.rhs=P.Variable rhs}, P.body=body, P.next=next}) ctx =
    ([ CMP(RegisterPlaceholder lhs, DataPlaceholder rhs) ], ctx)
  -- | 
  compile (P.InfixCall {P.lhs=P.Token(L.DigitLiteral(lhs)), P.function=func, P.rhs=P.Token(L.DigitLiteral(rhs))}) ctx = ([BL func], ctx)

  -- | 
  compile (P.InfixCall {P.lhs=P.Variable(lhs), P.function=func, P.rhs=P.Variable(rhs)}) ctx
    | func == "+"  = (asm ++ [ ADD  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    | func == "-"  = (asm ++ [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    | func == "*"  = (asm ++ [ MULS (R4, RegisterPlaceholder lhs, RegisterPlaceholder rhs ) ], ctx)
    | func == "/"  = (asm ++ [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    | func == "<"  = (asm ++ [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    | func == "==" = (asm ++ [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    | func == ">"  = (asm ++ [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    | func == "<=" = (asm ++ [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    | func == ">=" = (asm ++ [ SUB  (R4, RegisterPlaceholder lhs, DataPlaceholder rhs ) ], ctx)
    where
      lhsLoaded = ensureVariableLoaded lhs ctx
      rhsLoaded = lhsLoaded >>= (\(l,lc,la) -> ((ensureVariableLoaded rhs lc) >>= (\(r, rc, ra) -> Just (l, r, rc, la++ra) )))
      asm = case rhsLoaded of Just (_, _, _ , asm) -> asm; otherwise -> []
      registers = case rhsLoaded of Just (lr, rr, _, _) -> (lr, rr); otherwise -> (InvalidRegister, InvalidRegister)
      

    -- | otherwise    = ([ MOV  (R4, RegisterPlaceholder lhs), MOV(R1, RegisterPlaceholder rhs), BL func ], ctx) -- Otherwise call function with name. -- TODO FIX


  -- | 
  compile (P.SelfAssigningInfixCall {P.lhs=P.Variable(lhs), P.function=func, P.rhs=P.Variable(rhs)}) ctx
    | func == "+=" = (asm ++ [ ADD (lhsReg, lhsReg, Register rhsReg) ], ctx)
    | func == "-=" = (asm ++ [ SUB (lhsReg, lhsReg, Register rhsReg) ], ctx)
    | func == "*=" = (asm ++ [ MULS(RegisterPlaceholder lhs, RegisterPlaceholder lhs, RegisterPlaceholder rhs) ], ctx)
    | func == "/=" = (asm ++ [ SUB (lhsReg, lhsReg, Register rhsReg) ], ctx)
    | func == "/=" = (asm ++ [ SUB (lhsReg, lhsReg, Register rhsReg) ], ctx)
    | otherwise    = (asm ++ [ BL func ], ctx)
    where
      lhsLoaded = ensureVariableLoaded lhs ctx
      rhsLoaded = lhsLoaded >>= (\(l,lc,la) -> ((ensureVariableLoaded rhs lc) >>= (\(r, rc, ra) -> Just (l, r, rc, la++ra) )))
      asm = case rhsLoaded of Just (_, _, _ , asm) -> asm; otherwise -> []
      (lhsReg, rhsReg) = case rhsLoaded of Just (lr, rr, _, _) -> (lr, rr); otherwise -> (InvalidRegister, InvalidRegister)


  compile (P.SelfAssigningInfixCall {P.lhs=P.Variable(lhs), P.function=func, P.rhs=P.Nop}) ctx
    | lhsReg == InvalidRegister = trace ( "Invalid self assignment - " ++ lhs ++ " - " ++ show ctx) ([], ctx )
    | func == "++" = (asm ++ [ADD (lhsReg, lhsReg, Value 1)], ctx)
    | func == "--" = (asm ++ [SUB (lhsReg, lhsReg, Value 1)], ctx)
    where
      lhsLoaded = ensureVariableLoaded lhs ctx
      asm = case lhsLoaded of Just (_, _ , asm) -> asm; otherwise -> []
      lhsReg = case lhsLoaded of Just (lr, _, _) -> lr; otherwise -> InvalidRegister
    -- | otherwise    = [ LDR (R0, Right( DataPlaceholder lhs)), LDR (R1, Right( DataPlaceholder rhs)),  BL func ]

  compile (P.WhileStatement {P.condition=P.Variable{P.name=name}, P.body=body}) ctx =
      (conLoadAsm ++ [LABEL ".whileStart" ] ++ bodyInstructions ++ [LABEL ".whileStart"], bodyCtx)
      where 
        conLoadAsmResult = ensureVariableLoaded name ctx
        compReg = case conLoadAsmResult of Just (r, _, _) -> r; _ -> InvalidRegister
        conLoadAsm = case conLoadAsmResult of Just(_,_, asm) -> asm; _ -> []
        bodyAsmResult = conLoadAsmResult >>= (\(_,c, asm) -> Just (compile (P.Sequence body) c))
        (bodyCtx, bodyInstructions) = case bodyAsmResult of Just (c, asm) -> (asm,c); _ -> (ctx, [])


  compile (P.WhileStatement {P.condition=condition, P.body=body}) ctx =  (LABEL ".whileStart" : asmBody ++ asmCondition, newCtx)
    where
      (compiledCtx, asmCondition) = compileConditionalBranch  condition ".whileStart1" Nothing ctx
      (asmBody, newCtx) = compile (P.Sequence body) compiledCtx


  compile (P.FunctionReturn node) ctx = (childAsm ++ [MOV (PC, Register LR)], childCtx)
    where (childAsm, childCtx) = compile node ctx
  compile (P.FunctionCall{P.target=target, P.params=params}) ctx = ([ BL target ], ctx)
    -- where parameterInstructions

  -- compile (P.VarDeclaration{name})
  compile node ctx = trace ("Could not process node " ++ show node) ([ERROR ("Could not process node " ++ show node)], ctx)

  build :: P.FrogNode -> [ArmInstruction]
  build node = asm
    where (asm, _) = compile node initialCompilerContext
