module IlCompiler(compile, compileOptimize) where
    import qualified Ast as A
    import qualified Il as I
    import qualified Cpu as C
    import qualified IlCpuOptimizer as O
    import Debug.Trace (trace, traceShow)
    import Data.Maybe (fromMaybe, fromJust, catMaybes)
    import Data.List (elemIndex)

    compile :: A.FrogNode -> I.InstructionList
    compile n  =  trace ("[compile-buildStructure] " ++ show (buildStructure n) ++ "\n\n") (I.InstructionList (flattenStructure (map buildStack (buildStructure n))))

    compileOptimize :: C.Cpu -> A.FrogNode -> I.InstructionList
    compileOptimize c n = (O.fillRegisters c . I.InstructionList . flattenStructure . buildStructure) n





    -- prepareCondition ::  A.FrogNode  -> I.Instruction
    convertData :: A.FrogNode -> Maybe I.Data
    convertData (A.BooleanLiteral val) = Just (I.BooleanValue val)
    convertData (A.NumericLiteral f) =  Just (I.IntegerValue (round f))
    convertData (A.Variable v) = Just (I.Ref (I.Variable v))
    convertData _ = Nothing




    compileAssignment :: String -> A.FrogNode -> Maybe [ I.Instruction ]
    compileAssignment "" _ = Nothing
    compileAssignment var (A.BooleanLiteral b) = Just [ I.PushVar { I.store=I.Variable var, I.source = I.BooleanValue b} ]
    compileAssignment var (A.NumericLiteral f) = Just [ I.PushVar { I.store=I.Variable var, I.source = I.IntegerValue (round f)} ]
    compileAssignment var (A.IntegerLiteral i) = Just [ I.PushVar { I.store=I.Variable var, I.source = I.IntegerValue i }]
    compileAssignment var (A.Variable v) = Just [ I.PushVar { I.store=I.Variable var, I.source = I.Ref (I.Variable v)}]
    compileAssignment var infx@A.InfixCall {A.lhs=l, A.target=t, A.rhs=r}  = case (t, convertData l, convertData r, next) of
            ("+", Just lVal, Just rVal, Nothing) -> Just [ I.Add { I.store=I.Variable var, I.lhs=lVal, I.rhs=rVal}]
            ("+", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Add { I.store=I.Variable var, I.lhs=lVal, I.rhs=I.Ref (I.Variable var) }])
            ("-", Just lVal, Just rVal, Nothing) -> Just [ I.Sub { I.store=I.Variable var, I.lhs=lVal, I.rhs=rVal}]
            ("-", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Sub { I.store=I.Variable var, I.lhs=lVal, I.rhs=I.Ref (I.Variable var) }])
            ("*", Just lVal, Just rVal, Nothing) -> Just [ I.Mul { I.store=I.Variable var, I.lhs=lVal, I.rhs=rVal}]
            ("*", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Mul { I.store=I.Variable var, I.lhs=lVal, I.rhs=I.Ref (I.Variable var) }])
            ("/", Just lVal, Just rVal, Nothing) -> Just [ I.Div { I.store=I.Variable var, I.lhs=lVal, I.rhs=rVal}]
            ("/", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Div { I.store=I.Variable var, I.lhs=lVal, I.rhs=I.Ref (I.Variable var) }])
            _ -> Nothing
        where
            next = compileAssignment var r



    compileAssignment var node = trace ("[compileAssignment] Unknown pattern for variable= " ++ var ++ " => " ++ show node) Nothing

    compileIfCondition :: A.FrogNode -> Maybe [I.Instruction]
    compileIfCondition (A.Variable var) = Just [ I.Cmp {I.lhs=I.Ref (I.Variable var), I.rhs=I.BooleanValue True} ]
    compileIfCondition A.InfixCall{A.lhs=A.Variable l, A.target=t, A.rhs=A.IntegerLiteral r} = Just [ I.CmpStmtNext {I.lhs=I.Ref (I.Variable l), I.rhs=I.IntegerValue r, I.conditionType=conditionType} ]
        where
            conditionType = case t of
                "==" -> I.Equal
                "!=" -> I.NotEqual
                ">"  -> I.Greater
                "<"  -> I.Lesser
                ">="  -> I.GreaterEqual
                "<="  -> I.LesserEqual

    compileIfCondition _ = Nothing





    buildStructure :: A.FrogNode -> [I.Structure]
    -- buildStructure A.ConstDeclaration {A.name=n, A.typename=t, A.assignment=a} = case compileAssignment n a of
        -- Just instructions -> I.VariableScope { I.name=n, String String Structure}[ I.InstructionSequence instructions] 
        -- _ -> []


    buildStructure (A.Statement stmt) = buildStructure stmt
    buildStructure A.Assignment { A.target=t, A.assignment=a } = case compileAssignment t a of
        Just instructions -> [I.InstructionSequence instructions]
        _ -> []

    buildStructure A.InfixCall {A.lhs=l, A.target="+", A.rhs=r} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> [I.SingleInstruction I.Add{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR}]
        (Just dataL, Nothing) -> I.SingleInstruction I.Add{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : buildStructure r


    -- | Implement - operator
    buildStructure A.InfixCall {A.lhs=l, A.target="-", A.rhs=r} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> [I.SingleInstruction I.Sub{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR}]
        (Just dataL, Nothing) -> I.SingleInstruction I.Sub{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : buildStructure r

    buildStructure A.InfixCall {A.lhs=l, A.target="/", A.rhs=r} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> [I.SingleInstruction I.Div{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR}]
        (Just dataL, Nothing) -> I.SingleInstruction I.Div{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : buildStructure r

    buildStructure A.InfixCall {A.lhs=l, A.target="*", A.rhs=r} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> [I.SingleInstruction I.Mul{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR}]
        (Just dataL, Nothing) -> I.SingleInstruction I.Mul{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : buildStructure r


    buildStructure A.InfixCall { A.lhs=l, A.target=t, A.rhs=r} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> [I.SingleInstruction I.Call{I.routine=I.Label t, I.args=[dataL, dataR] } ] -- Reference ([Data]){I.store=I.Prev, I.lhs=dataL, I.rhs=dataR}]
        (Just dataL, Nothing) -> structR ++ [ I.SingleInstruction I.Call{I.routine=I.Label t, I.args=[dataL, I.Ref I.Prev] }]
        where structR = buildStructure r

    buildStructure A.Block { A.childeren=c } = [I.Scope (foldl1 (++) (map buildStructure c))]

    buildStructure A.Increment { A.lhs=A.Variable var} = [ I.SingleInstruction (I.Increment (I.Variable var)) ]
    buildStructure A.Decrement { A.lhs=A.Variable var} = [ I.SingleInstruction (I.Decrement (I.Variable var)) ]

    buildStructure A.SelfAssigningInfixCall { A.lhs=A.Variable var, A.target = t,  A.rhs = r } = case (convertData r, t) of
        (Just op, "+=") -> [ I.SingleInstruction (I.Add{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}) ]
        (Just op, "-=") -> [ I.SingleInstruction (I.Sub{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}) ]
        (Just op, "*=") -> [ I.SingleInstruction (I.Mul{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}) ]
        (Just op, "/=") -> [ I.SingleInstruction (I.Div{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}) ]
        (_, _) -> []

    buildStructure (A.FunctionReturn Nothing) = [ I.SingleInstruction I.RetNone ]
    buildStructure (A.FunctionReturn (Just val)) = case convertData val of
        Just value -> [ I.SingleInstruction (I.Ret value) ]
        _ -> [ I.SingleInstruction I.RetNone ]

    buildStructure (A.Sequence seq) = case seq of
        (A.VarDeclaration {A.name=n, A.assignment=a, A.typename=t} : xs) ->  [I.VariableScope {I.name=n, I.typename=t, I.assignment=[I.InstructionSequence (fromMaybe [] (compileAssignment n a))], I.body=buildStructure (A.Sequence xs)}]
        (A.ConstDeclaration  {A.name=n, A.assignment=a, A.typename=t} : xs) -> [I.ConstantScope {I.name=n, I.typename=t, I.assignment=[I.InstructionSequence (fromJust (compileAssignment n a))],  I.body=buildStructure (A.Sequence xs)}]
        (x:xs) -> buildStructure x ++ buildStructure (A.Sequence xs)
        [] -> []

    buildStructure A.RoutineDefinition {A.name=n, A.params=p, A.body=b} = [I.Routine { I.name=n, I.params=[], I.body= buildStructure (A.Sequence b)}]
    buildStructure A.FunctionDefinition {A.name=n, A.params=p, A.body=b} = [I.Routine { I.name=n, I.params=[], I.body= buildStructure (A.Sequence b)}]

    -- buildStructure A.IfStatement { A.condition = c, A.body=b, A.next=Nothing } = []

    buildStructure A.FunctionCall{ A.target=funcName, A.params=args} = case args of
        [] -> [ I.SingleInstruction I.Call{ I.storeReturn = Nothing, I.routine=I.Label funcName, I.args=[] } ]
        _ -> [ ]


    buildStructure A.IfStatement { A.condition = c, A.body=body, A.next= n} =
        case(c, n, ci) of

            (A.Variable v, Just next, _) -> [I.ConditionalBranch { I.condition=[I.SingleInstruction (I.Cmp { I.lhs=I.Ref (I.Variable v), I.rhs = I.BooleanValue True})], I.trueBranch=bInst, I.falseBranch=buildStructure next }]
            (A.Variable v, Nothing, _) -> [I.ConditionalBranch { I.condition=[I.SingleInstruction I.Cmp { I.lhs=I.Ref (I.Variable v), I.rhs = I.BooleanValue True}], I.trueBranch=bInst, I.falseBranch=[] }]
            (A.BooleanLiteral True, Just next, _) -> bInst
            (A.BooleanLiteral False, Just next, _) -> buildStructure next
            (A.BooleanLiteral False, Nothing, _) -> []
            (_, Just next, cinst) -> [I.ConditionalBranch { I.condition=cinst, I.trueBranch=bInst, I.falseBranch= buildStructure next }]
            (_, _, cinst) -> [I.ConditionalBranch { I.condition=cinst, I.trueBranch=bInst, I.falseBranch=[] }]
        where
            ci = case compileIfCondition c of
                Just [i] -> [I.SingleInstruction i]
                Just ins -> [I.InstructionSequence ins]
                _ -> []

            bInst = buildStructure (A.Sequence body)

    buildStructure A.WhileStatement { A.condition=A.BooleanLiteral True, A.body=b} = [ I.InfiniteLoop{ I.body= buildStructure (A.Sequence b) } ]

    buildStructure A.WhileStatement { A.condition=c, A.body=b} = case compileIfCondition c of
        Just [cInst] -> [ I.FiniteLoop {I.condition=[I.SingleInstruction cInst], I.body=buildStructure (A.Sequence b) } ]
        Just cInst -> [ I.FiniteLoop {I.condition=[I.InstructionSequence cInst], I.body=buildStructure (A.Sequence b) } ]
        Just [] -> [ I.InfiniteLoop { I.body=buildStructure (A.Sequence b) } ]
        _ -> []

    -- createStackFrames :: [ I.Structure ] -> [ I.Structure ]
    -- createStackFrames = createStackFramesImpl [ ]


    getSPOffset :: String -> [ String ] -> Maybe Int
    getSPOffset = elemIndex

    varToSPOffset :: I.Reference -> [ String ] -> Maybe I.Reference
    varToSPOffset (I.Variable var) stack = case getSPOffset var stack of
        Just off -> Just (I.StackPointerOffset off)
        Nothing -> Nothing
    varToSPOffset ref _ = Just ref





    buildStack :: I.Structure -> I.Structure
    buildStack = buildStackImpl []



    mapVarToStack :: [String] -> I.Reference -> I.Reference

    mapVarToStack stack (I.Variable var) = case offset of
            Just o -> I.StackPointerOffset o
            Nothing -> I.StackPointerOffset (-1)
        where
            offset = getSPOffset var stack
    mapVarToStack _ ref = ref




    buildStackImpl :: [String] -> I.Structure -> I.Structure
    buildStackImpl stack rt@I.Routine{I.body=b} = rt{I.body= map stackFunc b}
        where stackFunc = buildStackImpl stack
    buildStackImpl stack rt@I.ConditionalBranch{I.condition=c, I.trueBranch=t, I.falseBranch=f} = rt{I.condition=map stackFunc c, I.trueBranch=map stackFunc t, I.falseBranch=map stackFunc f}
        where stackFunc = buildStackImpl stack
    buildStackImpl stack b@I.Branch{I.match=m} = b{I.match=stackFunc m}
        where stackFunc = buildStackImpl stack
    buildStackImpl stack l@I.FiniteLoop{I.condition=c, I.body=b} =  l{I.body=map stackFunc b, I.condition=map stackFunc c}
        where
            stackFunc = buildStackImpl stack



    buildStackImpl stack l@I.InfiniteLoop{I.body=b} = l{I.body=map stackFunc b}
        where stackFunc = buildStackImpl stack
    buildStackImpl stack (I.Scope s) =  I.Scope (map stackFunc s)
        where stackFunc = buildStackImpl stack
    buildStackImpl stack vs@I.VariableScope{I.name=n, I.assignment=a, I.body=b} = (vs{I.assignment=map stackFunc a, I.body=map stackFunc b})
        where stackFunc = buildStackImpl (n:stack)
    buildStackImpl stack vs@I.ConstantScope{I.assignment=a, I.body=b} = (vs{I.assignment=map stackFunc a, I.body=map stackFunc b})
        where stackFunc = buildStackImpl stack

    -- buildStackImpl stack vs@I.ConstantScope{I.assignment=a, I.body=b} = (vs{I.assignment=map stackFunc a, I.body=map stackFunc b})
    --     where stackFunc = buildStackImpl stack



    buildStackImpl stack (I.InstructionSequence seq) =  (I.InstructionSequence . catMaybes . map (I.mapInstructionRefs func)) seq
        where
            func = mapVarToStack stack
            e = I.Error{I.name="Could not process instruction", I.body=[I.InstructionSequence seq]}

    buildStackImpl stack (I.SingleInstruction inst) = maybe e  I.SingleInstruction (I.mapInstructionRefs func inst)
        where
            func = mapVarToStack stack
            e = I.Error{I.name="Could not process instruction", I.body=[I.SingleInstruction inst]}



    flattenStructureImpl :: Int -> [ I.Structure ] -> (Int, [ I.Instruction ])

    flattenStructureImpl i ((I.SingleInstruction inst) : xs) = (newI, inst : next)
        where
            (newI, next) = flattenStructureImpl i xs

    flattenStructureImpl i ((I.InstructionSequence seq) : xs) = (newI, seq ++ next)
        where
            (newI, next) = flattenStructureImpl i xs


    flattenStructureImpl i (I.FiniteLoop { I.condition=c, I.body=b} : xs) = (nextI, I.Lbl ("loop_" ++ show i) : bodyInst ++ condInst ++ I.Jump (I.Label ("loop_" ++ show i)) : nextInst)
        where
            (condI, condInst) = flattenStructureImpl (i+1) c
            (bodyI, bodyInst) = flattenStructureImpl condI b
            (nextI, nextInst) = flattenStructureImpl bodyI xs

    flattenStructureImpl i (I.InfiniteLoop b: xs) = (nextI, I.Lbl ("loop_" ++ show i) : bodyInst ++ I.Jump (I.Label ("loop_" ++ show i))  : nextInst)
        where
            -- (condI, condInst) = flattenStructureImpl (i+1) b
            (bodyI, bodyInst) = flattenStructureImpl (i+1) b
            (nextI, nextInst) = flattenStructureImpl bodyI xs

    flattenStructureImpl i (I.Routine{I.name=n, I.body=b}: xs) = (nextI, (I.Lbl n : bInst) ++ [I.RetNone] ++ nextInst)
        where
        (bI, bInst) =  flattenStructureImpl i b
        (nextI, nextInst) = flattenStructureImpl bI xs

    flattenStructureImpl i (I.ConditionalBranch{I.condition=c, I.trueBranch=tb, I.falseBranch=fb} : xs) = case (tb, fb) of
        ([], []) -> (i, [])
        ([], fBranch) -> (falseI + 1, condtionInstr ++ [ I.BC { I.conditionType=I.Equal, I.target = I.Label ("endif_" ++ show i)}] ++ falseInst ++ [I.Lbl ("endif_" ++ show i)]  )
        (tBranch, []) -> (trueI + 1, condtionInstr ++ [ I.BC { I.conditionType=I.NotEqual, I.target = I.Label ("endif_" ++ show i)}] ++ trueInst ++ [I.Lbl ("endif_" ++ show i)]  )
        (tBranch, fBranch) -> (falseI + 1, condtionInstr ++ [ I.BC { I.conditionType=I.NotEqual, I.target = I.Label ("elseif_" ++ show i)}] ++ trueInst ++ [I.Jump (I.Label ("endif_" ++ show i)), I.Lbl ("ifelse" ++ show i)] ++ falseInst ++ [I.Lbl ("endif" ++ show i )] )
        where
            (conditionI, condtionInstr) = flattenStructureImpl (i + 1) c
            (trueI, trueInst) = flattenStructureImpl conditionI tb
            (falseI, falseInst) = flattenStructureImpl trueI fb




    flattenStructureImpl i [] = (i, [])
    flattenStructureImpl i (I.VariableScope {I.name=n, I.assignment=a, I.body=b} : xs) = (i, assignmentInst ++ bodyInst ++ nextInst ++ [I.PopVar [I.Variable n]])
        where
            (assignmenti, assignmentInst) = flattenStructureImpl i a
            (bodyI, bodyInst) = flattenStructureImpl assignmenti b
            (nextI, nextInst) = flattenStructureImpl bodyI xs

    flattenStructureImpl i n = trace ("[flattenStructureImpl] Unknown pattern => " ++ show n) (i, [])



    flattenStructure :: [ I.Structure ] -> [ I.Instruction ]
    flattenStructure struct = res
        where
            (_, res) = flattenStructureImpl 0 struct









    -- prepareForCpu :: [I.Structure] -> C.Cpu -> [I.Structure]
    -- prepareForCpu 





