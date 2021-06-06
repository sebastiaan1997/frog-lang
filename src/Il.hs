
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Il(Reference(..), Data(..), Instruction(..), BranchType(..), Structure(..), InstructionList(..), mapInstructionsInStructure, Register(..), mapInstructionListRefs, mapInstructionRefs, extractInstructionData, replaceInstructionData, dataRef, dataRefPayload, mapInstructionRefPayload, ILDocument(..), invertCondition) where

    import Numeric (showHex)
    import qualified Data.Data as D
    import qualified Data.Map as M
    import Data.Maybe(mapMaybe, catMaybes, isNothing, fromMaybe)

    newtype Register =  Register Int
        deriving (Eq, D.Data)
    
    instance Show Register where
        show (Register r) = 'R' : show r
    data Reference = Label String
        | DataLabel String
        | R Register
        | Offset (Reference, Int)
        | StackframeOffset { context :: String, offset :: Int}
        | StackPointerOffset Int
        | Absolute Int
        | Variable String
        | Prev -- Defines that the previous instruction provides this value.
        | Next -- Defines that the next instruction needs the current value.
        | Parent -- Defines if the value should be stored as defined by its parent
        | ReturnValue
        deriving ( Eq, D.Data )

    instance Show Reference where
        show (R r) = show  r
        show (Label lbl) = '.' : lbl
        show (StackPointerOffset o) = "[sp, #" ++ show o ++ "]"
        show StackframeOffset{context=ctx, offset=o} = '"' : ctx ++ "\"->[ sp, " ++ show o ++ "]"
        show (Absolute addr) =  "0x" ++ showHex addr ""
        show (Variable var)  = '`' : var ++ "`"
        show (Offset (ref, o)) = '[' : show ref ++ ", " ++ show o ++ "]"
        show Prev = "prev"
        show Next = "next"
        show Parent = "parent"
        show ReturnValue = "return_value"

    data Data = Ref Reference
        | IntegerValue Int
        | BinaryType { value :: Int, size :: Int }
        | BooleanValue Bool
        | ByteArray String
        | IVector [ Int ]
        deriving ( Eq, D.Data )

    instance Show Data where
        show (Ref ref) = "&" ++ show ref
        show (IntegerValue val) = show val
        show BinaryType {value=v, size=s} = '{' : show v ++ ';' : show s ++ "}"
        show (BooleanValue b) = if b then "true" else "false"
        show (ByteArray arr) = arr
        show (IVector val) = show val


    data Instruction = Call { storeReturn :: Maybe Reference, routine :: Reference, args :: [ Data ] }
        | Goto Reference
        | CmpStmtNext   { lhs :: Data, rhs :: Data, conditionType :: BranchType }
        | Cmp           { lhs :: Data, rhs :: Data } -- Compare two values for next instruction
        | CmpStore      { store :: Reference, lhs :: Data, rhs :: Data }
        | Add           { store :: Reference, lhs :: Data, rhs :: Data }
        | Sub           { store :: Reference, lhs :: Data, rhs :: Data }
        | Mul           { store :: Reference, lhs :: Data, rhs :: Data }
        | Div           { store :: Reference, lhs :: Data, rhs :: Data }
        | B             { conditionType :: BranchType, lhs :: Data, rhs :: Data, target :: Reference }
        | BC            { conditionType :: BranchType, target :: Reference }
        | Increment     Reference -- Increment by 1
        | Decrement     Reference -- Decrement by 1
        | Jump Reference
        | Load Reference
        | Store Reference
        | LoadToReg (Register, Reference)
        | StoreFromReg (Register, Reference)
        | Create Reference
        | Push Data
        | PushVar { store :: Reference, source :: Data }
        | Pop
        | PopVar [Reference]
        | Remove Reference
        | Sum [Data]
        | Set { store :: Reference, source :: Data }
        | Lbl String
        | Ret Data
        | RetNone
        | RoutineInit [Reference]
        | RToStack [Register]
        | StackToR [Register]
        deriving ( Eq, D.Data )

    replaceInstructionData :: Instruction -> [Maybe Data] -> Maybe Instruction
    replaceInstructionData c@Call{} (Just (Ref sr): Just (Ref rt): xs) = Just c{storeReturn=Just sr, routine=rt, args=catMaybes xs}
    replaceInstructionData c@Call{} (Nothing: Just (Ref rt): xs) = Just c{storeReturn=Nothing, routine=rt, args=catMaybes xs}
    replaceInstructionData i@Goto{} [Just (Ref ref)] = (Just . Goto) ref
    replaceInstructionData i@CmpStmtNext{} [Just l, Just r] = Just i{lhs=l, rhs=r}
    replaceInstructionData i@CmpStore{} [Just (Ref s), Just l, Just r] = Just i{store=s, lhs=l, rhs=r}
    replaceInstructionData i@Cmp{} [Just l, Just r]= Just i{lhs=l, rhs=r}
    replaceInstructionData i@Add{} [Just (Ref s), Just l, Just r]  = Just i{lhs=l, rhs=r, store=s}
    replaceInstructionData i@Sub{} [Just (Ref s), Just l,Just r] = Just i{lhs=l, rhs=r, store=s}
    replaceInstructionData i@Mul{} [Just (Ref s), Just l,Just r] = Just i{lhs=l, rhs=r, store=s}
    replaceInstructionData i@Div{} [Just (Ref s), Just l,Just r] = Just i{lhs=l, rhs=r, store=s}
    replaceInstructionData i@B{} [Just (Ref t), Just l,Just r] = Just i{lhs=l, rhs=r, target=t}
    replaceInstructionData i@BC{} [Just (Ref t)] = Just i{target=t}
    replaceInstructionData (Jump _) [Just (Ref ref)] = (Just . Jump) ref
    replaceInstructionData (Load _) [Just (Ref ref)] = (Just . Load) ref
    replaceInstructionData (Store _) [Just (Ref ref)] = (Just . Store) ref
    replaceInstructionData (Create _) [Just (Ref ref)] = (Just . Create) ref
    replaceInstructionData (Push _) [Just d] = (Just . Push) d
    replaceInstructionData (LoadToReg (_,_)) [Just (Ref (R reg)), Just (Ref ref)] = (Just . LoadToReg) (reg, ref)
    replaceInstructionData (StoreFromReg (_,_)) [Just (Ref (R reg)), Just (Ref ref)] = (Just . StoreFromReg) (reg, ref)
    replaceInstructionData i@PushVar{} [Just (Ref st), Just src] =  Just i{store=st, source=src}
    replaceInstructionData i@Set{} [Just (Ref st), Just src] =  Just i{store=st, source=src}
    replaceInstructionData (PopVar _) vars = (Just . PopVar . mapMaybe filterFunc) vars
        where
                filterFunc = \case Just (Ref r) -> Just r; _ -> Nothing
    replaceInstructionData (Remove _) [Just (Ref ref)] = (Just . Remove) ref
    replaceInstructionData (Increment _) [Just (Ref r)] = (Just . Increment) r
    replaceInstructionData (Decrement _) [Just (Ref r)] = (Just . Decrement) r
    replaceInstructionData (Sum _) [] = Nothing
    replaceInstructionData (Sum _) args = (Just . Sum . catMaybes) args
    replaceInstructionData (Ret _) [Just ret] = (Just . Ret) ret
    replaceInstructionData _ _ = Nothing

    extractInstructionData :: Instruction -> [Maybe Data]
    extractInstructionData Call{storeReturn=sr, routine=rt, args=a} = ((Just . Ref) =<< sr) : (Just . Ref) rt : map Just a
    extractInstructionData (Goto ref) = [(Just . Ref) ref]
    extractInstructionData CmpStmtNext{lhs=l, rhs=r} = map Just [l,r]
    extractInstructionData CmpStore{lhs=l, rhs=r, store=s} = map Just [Ref s, l,r]
    extractInstructionData Cmp{lhs=l, rhs=r} = map Just [l,r]
    extractInstructionData Add{lhs=l, rhs=r, store=s} = map Just [Ref s, l,r]
    extractInstructionData Sub{lhs=l, rhs=r, store=s} = map Just [Ref s, l,r]
    extractInstructionData Mul{lhs=l, rhs=r, store=s} = map Just [Ref s, l,r]
    extractInstructionData Div{lhs=l, rhs=r, store=s} = map Just [Ref s, l,r]
    extractInstructionData B{lhs=l, rhs=r, target=t} = map Just [Ref t, l,r]
    extractInstructionData BC{target=t} = [(Just . Ref) t]
    extractInstructionData (Increment i) = [(Just . Ref) i]
    extractInstructionData (Decrement d) = [(Just . Ref) d]
    extractInstructionData (Jump j) = [(Just . Ref) j]
    extractInstructionData (Load l) = [(Just . Ref) l]
    extractInstructionData (Store l) = [(Just . Ref) l]
    extractInstructionData (LoadToReg (reg, ref)) = map (Just . Ref) [R reg, ref]
    extractInstructionData (StoreFromReg (reg, ref)) = map (Just .  Ref) [R reg, ref]
    extractInstructionData (Create ref) = [(Just . Ref) ref]
    extractInstructionData (Push d) = [Just d]
    extractInstructionData PushVar{store=st, source=s} = map Just [Ref st, s]
    extractInstructionData (PopVar v) = map (Just . Ref) v
    extractInstructionData (Remove ref) = [(Just . Ref) ref]
    extractInstructionData (Sum d) = map Just d
    extractInstructionData Set{source=src, store=st} = map Just [Ref st, src]
    extractInstructionData (Ret ret) = [Just ret]
    extractInstructionData _ = []




    dataRefPayload :: (Reference -> (Reference, a)) -> Data -> Maybe (Data, a)
    dataRefPayload func (Ref ref) =  Just (Ref newRef, p)
        where
            (newRef, p) = func ref
    dataRefPayload _ _ = Nothing


    dataRef:: (Reference -> Reference) -> Data -> Data
    dataRef func (Ref r) = (Ref . func) r
    dataRef _ d = d


    mapInstructionRefPayload :: (Reference -> (Reference, a)) -> Instruction -> Maybe (Instruction, [a])
    mapInstructionRefPayload func instr =  (\x -> Just(x, payload)) =<< newInstruction
        where
            result = (map (maybe Nothing (dataRefPayload func)) . extractInstructionData) instr
            payload = (map snd . catMaybes) result
            newInstruction = (replaceInstructionData instr . map (fmap fst)) result


    mapInstructionDataPayload :: (Data -> (Data, a)) -> Instruction -> Maybe (Instruction, [a])
    mapInstructionDataPayload func instr =  (\x -> Just(x, payload)) =<< newInstruction
        where
            result = (map (fmap func) . extractInstructionData) instr
            payload = (map snd . catMaybes) result
            newInstruction = (replaceInstructionData instr . map (fmap fst)) result

    

    mapInstructionData :: (Data -> Data) -> Instruction -> Maybe Instruction
    mapInstructionData func instr = (replaceInstructionData instr . map (fmap func) . extractInstructionData) instr

    mapInstructionRefs :: (Reference -> Reference) -> Instruction -> Maybe Instruction
    mapInstructionRefs func = mapInstructionData (\case Ref r -> (Ref .func) r; other -> other)

    mapInstructionListRefs :: (Reference -> Reference) -> InstructionList -> Maybe InstructionList
    mapInstructionListRefs func (InstructionList lst)
        | any isNothing d = Nothing
        | otherwise = (Just . InstructionList . catMaybes) d
        where
            d = map (mapInstructionRefs func) lst

    instance Show Instruction where
        show Call { storeReturn=r, routine=target, args=a} = "\tCALL " ++ show r ++ ", " ++ show target ++ ", " ++ show a
        show CmpStmtNext{ lhs=l, rhs=r, conditionType=ct} = "\tCMPSN " ++ show l ++ ", " ++ show r ++ ", " ++ show ct
        show (Goto ref) = "\tGOTO " ++ show ref
        show Cmp{lhs=l, rhs=r} = "\tCMP " ++ show l ++ ", " ++ show r
        show CmpStore{store=s, lhs=l, rhs=r} = "\tCMPSTORE " ++ show s  ++ ", " ++ show l ++ ", " ++ show r
        show Add{store=s, lhs=l, rhs=r} = "\tADD " ++ show s ++ ", " ++ show l ++ ", " ++ show r
        show Sub{store=s, lhs=l, rhs=r} = "\tSUB " ++ show s ++ ", " ++ show l ++ ", " ++ show r
        show Mul{store=s, lhs=l, rhs=r} = "\tMUL " ++ show s ++ ", " ++ show l ++ ", " ++ show r
        show Div{store=s, lhs=l, rhs=r} = "\tDIV " ++ show s ++ ", " ++ show l ++ ", " ++ show r
        show B{conditionType=ct, lhs=l, rhs=r, target=t} = "\tB " ++ show ct ++ ", " ++ show l ++ ", " ++ show r ++ ", " ++ show t
        show BC{conditionType=ct, target=t} = "\tBC " ++ show ct ++ ", " ++ show t
        show Pop = "\tPOP"
        show (Push ref) = "\tPUSH" ++ show ref
        show PushVar{store=sto, source=src}    = "\tPUSHVAR " ++ show sto ++ ", " ++ show src
        show (PopVar var)    = "\tPOPVAR " ++ show var
        show (Increment i) = "\tINC " ++ show i
        show (Decrement d) = "\tDEC " ++ show d
        show (Jump target) = "\tJUMP " ++ show target
        show (Load ref)    = "\tLOAD " ++ show ref
        show (Store ref)    = "\tSTORE " ++ show ref
        show (Create ref)   = "\tCREATE " ++ show ref
        show (Remove ref)   = "\tREM " ++ show ref
        show Set{store=sto, source=src}    = "\tSET " ++ show sto ++ ", " ++ show src
        show (Lbl l) = '.' : l
        show (Ret t) = "\tRET " ++ show t
        show RetNone = "\tRETNONE"
        show (LoadToReg(st, src))  = "\tLDREG " ++ show st ++ ", " ++ show src
        show (StoreFromReg(st, src)) = "\tLSTOREG " ++ show st ++ ", " ++ show src
        show (RoutineInit args) = "\tROUTINE_INIT [" ++ (foldr (\l r -> show l ++ ", "++ r ) "" args) ++ "]"
        show (RToStack registers) = "\tRTOSTACK [" ++ (foldr (\l r -> show l ++ r ) "" registers)  ++ "]"
        show (StackToR registers) = "\tSTACKTOR [" ++ (foldr (\l r -> show l ++ r ) "" registers) ++ "]"

    newtype InstructionList = InstructionList [ Instruction ]

    instance Show InstructionList where
        show (InstructionList lst) = foldl (\l r -> l ++ show r ++ "\n") " " lst


    data ILDocument = ILDocument {exports :: [String], rw :: M.Map String Data, ro :: M.Map String Data, instructions :: InstructionList}
        deriving ( Show )

    data BranchType = Unconditional -- Branch should be executed unconditionally.
        | Greater -- Branch should be executed, only if the compare condition equals to greater than
        | GreaterEqual -- Branch should be executed, only if the compare condition equals to greater than or equal
        | Lesser -- Branch should be executed, only if the compare condition equals to lesser than
        | LesserEqual -- Branch should be executed, only if the compare condition equals to lesser than or equal.
        | Equal -- Branch should be executed, only if the compare condition equals to equal.
        | NotEqual -- Branch should be executed, only if the compare condition equals to not equal.
        | Undefined -- Branch condition is undefined, undefined behavriour 
        deriving ( Eq, Show, D.Data )

    invertCondition :: BranchType -> BranchType
    invertCondition Greater = LesserEqual
    invertCondition GreaterEqual = Lesser
    invertCondition Lesser = GreaterEqual
    invertCondition LesserEqual = Greater
    invertCondition Equal = NotEqual
    invertCondition NotEqual = Equal

    invertCondition x = x

    data Structure = SingleInstruction Instruction -- Represents a single instruction
        | InstructionSequence [ Instruction ] -- Represents multiple registers.
        | Routine { name :: String, body :: [Structure], params :: [ String ] } -- Represents a routine.
        | ConditionalBranch { condition :: [ Structure ], trueBranch :: [ Structure ] , falseBranch :: [ Structure ] } -- Represents branch with two possible flows of execution.
        | Branch { branchType :: BranchType, compareArgs :: (Reference, Reference), match :: Structure  } --  Represents a branch with one flow of execution
        | FiniteLoop { condition :: [ Structure ], body :: [Structure] } -- Represents a loop with a given end condition.
        | InfiniteLoop { body :: [Structure] } -- Represents an endless loop.
        | Scope [ Structure ] -- Represents a scope.
        | VariableScope { name :: String, typename :: String, assignment :: [Structure], body :: [Structure]} -- Represents a scope of a variable.
        | ConstantScope { name :: String, typename :: String, assignment :: [Structure], body :: [Structure]} -- Represents a scope of a constant.
        | Error { name :: String, body :: [Structure]} -- Represents an error.
        deriving ( Eq, Show )

    -- | Maps all sub structures to a given function.
    mapStructure :: (Structure -> Structure) -> Structure -> Structure
    mapStructure func rt@Routine{body=b} = func (rt{body= map func b}) -- Maps routine and body
    mapStructure func rt@ConditionalBranch{trueBranch=t, falseBranch=f} = func (rt{trueBranch=map func t, falseBranch=map func f}) -- Maps the Conditional branch, the true branch and the false branch
    mapStructure func b@Branch{match=m} = func b{match=func m} -- Maps the match and the branch
    mapStructure func l@FiniteLoop{body=b, condition=cond} = func l{condition=map func cond,body=map func b} -- Maps FiniteLoop the body and the condition
    mapStructure func l@InfiniteLoop{body=b} = func l{body=map func b} --Maps InfiniteLoop and body
    mapStructure func (Scope s) = func (Scope (map func s)) -- Maps scope
    mapStructure func vs@VariableScope{assignment=a, body=b} = func (vs{assignment=map func a, body=map func b}) -- Maps variable scope, assignment and body
    mapStructure func vs@ConstantScope{assignment=a, body=b} = func (vs{assignment=map func a, body=map func b}) -- Maps contstant scope, assignment and body
    mapStructure func struct = func struct -- Just map the structure.

    -- | Maps instructions in structure.
    mapInstructionsInStructure :: (Instruction -> Instruction) -> Structure -> Structure
    mapInstructionsInStructure func = mapStructure newFunc -- Maps instructions
        where
            newFunc = \case
                SingleInstruction si -> SingleInstruction (func si)
                InstructionSequence is -> InstructionSequence (map func is)
                other -> other
    





