
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Il(Reference(..), Data(..), Instruction(..), BranchType(..), Structure(..), InstructionList(..), mapInstructionRef, mapInstructionsInStructure, mapRefs, Register(..)) where

    import Numeric (showHex)
    import qualified Data.Data as D

    newtype Register =  Register Int
        deriving (Eq, Show, D.Data)
    data Reference = Label String
        | R Register
        | Offset (Reference, Int)
        | StackframeOffset { context :: String, offset :: Int}
        | StackPointerOffset Int
        | Absolute Int
        | Variable String
        | Prev -- Defines that the previous instruction provides this value.
        | Next -- Defines that the next instruction needs the current value.
        | Parent -- Defines if the value should be stored as defined by its parent
        deriving ( Eq, D.Data )

    instance Show Reference where
        show (Label lbl) = '.' : lbl
        show (StackPointerOffset o) = "[sp, #" ++ show o ++ "]"
        show StackframeOffset{context=ctx, offset=o} = '"' : ctx ++ "\"->[ sp, " ++ show o ++ "]"
        show (Absolute addr) =  "0x" ++ showHex addr ""
        show (Variable var)  = '`' : var ++ "`"
        show (Offset (ref, o)) = '[' : show ref ++ ", " ++ show o ++ "]"
        show Prev = "prev"
        show Next = "next"
        show Parent = "parent"






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
        | Set { store :: Reference, source :: Data }
        | Lbl String
        | Ret Data
        | RetNone
        deriving ( Eq, D.Data )

    hasField :: D.Data a => String -> a -> Bool
    hasField  name instr  = name `elem` D.constrFields(D.toConstr instr)

    tryGet :: D.Data a => String -> (a -> b) -> a -> Maybe b
    tryGet name accessor t
        | hasField name t   = Just (accessor t)
        | otherwise = Nothing






    mapInstructionRefExtra :: (Reference -> (a, Reference)) -> Instruction -> ([a], Instruction)
    mapInstructionRefExtra func inst =  ([], inst)
        where
            (maybeLhs, maybeRhs, maybeStore) = (tryGet "lhs" lhs inst, tryGet "rhs" rhs inst , tryGet "store" store inst )
            newLhs = case newLhs of
                Just l -> let (r, ref) func l in ([r], [ref])
                
            










    mapInstructionRefExtra func inst = case inst of
        -- call@Call{storeReturn=Just r, routine=rt, args=a} -> call{ storeReturn=Just (func r), routine=func rt }
        -- call@Call{storeReturn=Nothing, routine=rt, args=a} -> call{ routine=func rt }
        -- (Goto g) -> Goto (func g)
        -- cmp@CmpStmtNext{lhs=Ref l, rhs=Ref r} -> cmp{lhs=Ref (func l), rhs=Ref (func r)}

        -- cmp@CmpStmtNext{lhs=Ref l} -> cmp{lhs=Ref (func l)}
        -- cmp@CmpStmtNext{rhs=Ref r} -> cmp{rhs=Ref (func r)}
        cmp@Cmp{lhs=Ref l, rhs=Ref r} -> cmp{lhs=Ref (func l), rhs=Ref (func r)}
        cmp@Cmp{lhs=Ref l} -> cmp{lhs=Ref (func l)}
        cmp@Cmp{rhs=Ref r} -> cmp{ rhs=Ref (func r)}
        cmp@CmpStore{store=s, lhs=Ref l, rhs=Ref r} -> cmp{store=func s, lhs=Ref (func l), rhs=Ref (func r) }
        cmp@CmpStore{store=s, lhs=Ref l } -> cmp{ store = func s, lhs=Ref (func l) }
        cmp@CmpStore{store=s, rhs=Ref r } -> cmp{ store = func s, rhs=Ref (func r) }
        add@Add{ store=s, lhs=Ref l, rhs=Ref r } -> add{ store=func s, lhs = Ref (func l), rhs = Ref (func r)}
        add@Add{ store=s, lhs=Ref l } -> add{ store=func s, lhs = Ref (func l) }
        add@Add{ store=s, rhs=Ref r } -> add{ store=func s, rhs = Ref (func r) }
        sub@Sub{ store=s, lhs=Ref l, rhs=Ref r } -> sub{ store=func s, lhs = Ref (func l), rhs = Ref (func r)}
        sub@Sub{ store=s, lhs=Ref l } -> sub{ store=func s, lhs = Ref (func l) }
        sub@Sub{ store=s, rhs=Ref r } -> sub{ store=func s, rhs = Ref (func r) }
        mul@Mul{ store=s, lhs=Ref l, rhs=Ref r } -> mul{ store=func s, lhs = Ref (func l), rhs = Ref (func r)}
        mul@Mul{ store=s, lhs=Ref l } -> mul{ store=func s, lhs = Ref (func l) }
        mul@Mul{ store=s, rhs=Ref r } -> mul{ store=func s, rhs = Ref (func r) }
        div@Div{ store=s, lhs=Ref l, rhs=Ref r } -> div{ store=func s, lhs = Ref (func l), rhs = Ref (func r)}
        div@Div{ store=s, lhs=Ref l } -> div{ store=func s, lhs = Ref (func l) }
        div@Div{ store=s, rhs=Ref r } -> div{ store=func s, rhs = Ref (func r) }
        (Increment inc) -> Increment (func inc)
        (Decrement dec) -> Increment (func dec)
        (Jump j) -> Jump (func j)
        (Load l) -> Load (func l)
        (Store s) -> Store (func s)
        (Create r) -> Create(func r)
        (Remove r) -> Remove(func r)
        Set{store=st, source=Ref so} -> Set{store=func st, source=Ref (func so)}
        s@Set{store=st} -> s{store=func st}
        (Ret (Ref val)) -> Ret (Ref (func val))
        inst -> inst



    mapInstructionRef :: (Reference -> Reference) -> Instruction -> Instruction
    mapInstructionRef func inst = snd (mapInstructionRefExtra (\x -> (Nothing, func x)) inst)













    mapRefs :: (Reference -> Reference) -> InstructionList -> InstructionList
    mapRefs func (InstructionList lst) = InstructionList (map (mapInstructionRef func) lst)




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
        show (Ret t) = "\tRET" ++ show t
        show RetNone = "\tRETNONE"







    newtype InstructionList = InstructionList [ Instruction ]




    instance Show InstructionList where
        show (InstructionList lst) = foldl (\l r -> l ++ show r ++ "\n") " " lst










    data BranchType = Unconditional
        | Greater
        | GreaterEqual
        | Lesser
        | LesserEqual
        | Equal
        | NotEqual
        | Undefined
        deriving ( Eq, Show )

    data Structure = SingleInstruction Instruction
        | InstructionSequence [ Instruction ]
        | Routine { name :: String, body :: [Structure], params :: [ String ] }
        | ConditionalBranch { condition :: [ Structure ], trueBranch :: [ Structure ] , falseBranch :: [ Structure ] }
        | Branch { branchType :: BranchType, compareArgs :: (Reference, Reference), match :: Structure  }
        | FiniteLoop { condition :: [ Structure ], body :: [Structure] }
        | InfiniteLoop { body :: [Structure] }
        | Scope [ Structure ]
        | VariableScope { name :: String, typename :: String, assignment :: [Structure], body :: [Structure]}
        | ConstantScope { name :: String, typename :: String, assignment :: [Structure], body :: [Structure]}
        deriving ( Eq, Show )

    mapStructure :: (Structure -> Structure) -> Structure -> Structure

    mapStructure func rt@Routine{body=b} = func (rt{body= map func b})
    mapStructure func rt@ConditionalBranch{trueBranch=t, falseBranch=f} = func (rt{trueBranch=map func t, falseBranch=map func f})
    mapStructure func b@Branch{match=m} = func b{match=func m}
    mapStructure func l@FiniteLoop{body=b, condition=cond} = func l{condition=map func cond,body=map func b}
    mapStructure func l@InfiniteLoop{body=b} = func l{body=map func b}
    mapStructure func (Scope s) = func (Scope (map func s))
    mapStructure func vs@VariableScope{assignment=a, body=b} = func (vs{assignment=map func a, body=map func b})
    mapStructure func vs@ConstantScope{assignment=a, body=b} = func (vs{assignment=map func a, body=map func b})
    mapStructure func struct = func struct


    mapInstructionsInStructure :: (Instruction -> Instruction) -> Structure -> Structure
    mapInstructionsInStructure func = mapStructure newFunc
        where
            newFunc = \case
                SingleInstruction si -> SingleInstruction (func si)
                InstructionSequence is -> InstructionSequence (map func is)
                other -> other

    mapStructureRefs :: (Reference -> Reference) -> Structure -> Structure
    mapStructureRefs func = mapInstructionsInStructure (mapInstructionRef func)




