module Il(Reference(..), Data(..), Instruction(..), BranchType(..), Structure(..), InstructionList(..)) where

    import Numeric (showHex)
    data Reference = Label String
        | Offset (Reference, Integer)
        | StackframeOffset { context :: String, offset :: Integer}
        | StackPointerOffset Integer
        | Absolute Integer
        | Variable String
        | Prev -- Defines that the previous instruction provides this value.
        | Next -- Defines that the next instruction needs the current value.
        | Parent -- Defines if the value should be stored as defined by its parent
        deriving ( Eq )

    instance Show Reference where
        show (Label lbl) = '.' : lbl
        show StackframeOffset{context=ctx, offset=o} = '"' : ctx ++ "\"->[ sp, " ++ show o ++ "]"
        show (Absolute addr) =  "0x" ++ showHex addr ""
        show (Variable var)  = '@' : var
        show (Offset (ref, o)) = '[' : show ref ++ ", " ++ show o ++ "]"
        show Prev = "prev"
        show Next = "next"
        show Parent = "parent"






    data Data = Ref Reference
        | IntegerValue Int
        | BinaryType { value :: Int, size :: Int }
        | BooleanValue Bool
        | ByteArray [Char]
        | IVector [ Int ]
        deriving ( Eq )

    instance Show Data where
        show (Ref ref) = "%" ++ show ref
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
        | Increment     { store :: Reference } -- Increment by 1
        | Decrement     { store :: Reference } -- Decrement by 1
        | Jump Reference
        | Load Reference
        | Store Reference
        | Remove Reference
        | Set { store :: Reference, source :: Data }
        | Lbl String
        | Ret Data
        | RetNone
        deriving ( Eq )

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
        show Increment{store=s} = "\tINC " ++ show s
        show Decrement{store=s} = "\tDEC " ++ show s
        show (Jump target) = "\tJUMP " ++ show target
        show (Load ref)    = "\tLOAD " ++ show ref
        show (Store ref)    = "\tSTORE " ++ show ref
        show (Remove ref)   = "\tREM " ++ show ref 
        show Set{store=sto, source=src}    = "\tSTORE " ++ show sto ++ ", " ++ show src
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
        | ConditionalBranch { condition :: [ Instruction ], trueBranch :: [ Structure ] , falseBrach :: [ Structure ] }
        | Branch { branchType :: BranchType, compareArgs :: (Reference, Reference), match :: Structure  }
        | FiniteLoop { condition :: [ Instruction ], body :: [Structure] }
        | InfiniteLoop { body :: [Structure] }
        | Scope [ Structure ]
        | VariableScope { name :: String, typename :: String, assignment :: [Structure], body :: [Structure]}
        | ConstantScope { name :: String, typename :: String, assignment :: [Structure], body :: [Structure]}
        deriving ( Eq, Show )
