module IlCompiler(compile) where
    import qualified Ast as A
    import qualified Il as I
    import Debug.Trace (trace, traceShow)
    import Data.Maybe (fromMaybe, fromJust)
    import Data.List (elemIndex)

    compile :: A.FrogNode -> I.InstructionList
    compile n  =  trace ("[compile-buildStructure] " ++ show (buildStructure n) ++ "\n\n") (I.InstructionList (flattenStructure (buildStructure n)))


    -- prepareCondition ::  A.FrogNode  -> I.Instruction
    convertData :: A.FrogNode -> Maybe I.Data
    convertData (A.BooleanLiteral val) = Just (I.BooleanValue val)
    convertData (A.NumericLiteral f) =  Just (I.IntegerValue (round f))
    convertData (A.Variable v) = Just (I.Ref (I.Variable v))
    convertData _ = Nothing




    compileAssignment :: String -> A.FrogNode -> Maybe [ I.Instruction ]
    compileAssignment "" _ = Nothing
    compileAssignment var (A.BooleanLiteral b) = Just [ I.Set { I.store=I.Variable var, I.source = I.BooleanValue b} ]
    compileAssignment var (A.NumericLiteral f) = Just [ I.Set { I.store=I.Variable var, I.source = I.IntegerValue (round f)} ]
    compileAssignment var (A.IntegerLiteral i) = Just [ I.Set { I.store=I.Variable var, I.source = I.IntegerValue i }]
    compileAssignment var (A.Variable v) = Just [ I.Set { I.store=I.Variable var, I.source = I.Ref (I.Variable v)}]
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

            (A.Variable v, Just next, _) -> [I.ConditionalBranch { I.condition=[I.Cmp { I.lhs=I.Ref (I.Variable v), I.rhs = I.BooleanValue True}], I.trueBranch=bInst, I.falseBrach=buildStructure next }]
            (A.Variable v, Nothing, _) -> [I.ConditionalBranch { I.condition=[I.Cmp { I.lhs=I.Ref (I.Variable v), I.rhs = I.BooleanValue True}], I.trueBranch=bInst, I.falseBrach=[] }]
            (A.BooleanLiteral True, Just next, _) -> bInst
            (A.BooleanLiteral False, Just next, _) -> buildStructure next
            (A.BooleanLiteral False, Nothing, _) -> []
            (_, Just next, Just cinst) -> [I.ConditionalBranch { I.condition=cinst, I.trueBranch=bInst, I.falseBrach= buildStructure next }]
            (_, _, Just cinst) -> [I.ConditionalBranch { I.condition=cinst, I.trueBranch=bInst, I.falseBrach=[] }]
        where
            ci = compileIfCondition c
            bInst = buildStructure (A.Sequence body)

    buildStructure A.WhileStatement { A.condition=A.BooleanLiteral True, A.body=b} = [ I.InfiniteLoop{ I.body= buildStructure (A.Sequence b) } ]

    buildStructure A.WhileStatement { A.condition=c, A.body=b} = case compileIfCondition c of
        Just cInst -> [ I.FiniteLoop {I.condition=cInst, I.body=buildStructure (A.Sequence b) } ]
        _ -> []

    createStackFrames :: [ I.Structure ] -> [ I.Structure ]
    createStackFrames = createStackFramesImpl [ ]


    getSPOffset :: String -> [ String ] -> Maybe Int
    getSPOffset var stack = case elemIndex var stack  of
        Just i -> Just (length stack - i)
        Nothing -> Nothing

    varToSPOffset :: I.Reference -> [ String ] -> Maybe I.Reference 
    varToSPOffset (I.Variable var) stack = case getSPOffset var stack of 
        Just off -> Just (I.StackPointerOffset off)
        Nothing -> Nothing
    varToSPOffset ref _ = Just ref


    createStackFramesImpl :: [ String ] -> [ I.Structure ] -> [ I.Structure ]
    createStackFramesImpl stack (rt@I.Routine {I.name=n, I.body=b, I.params=p}: xs) = rt{I.body=newBody} : createStackFramesImpl  stack xs
        where
            newBody = createStackFramesImpl p b

    

    

    createStackFramesImpl stack (add@I.Add{I.lhs=l, I.rhs=r}: xs) = case (l, r) of
        (I.Ref refLeft, I.Ref refRight) -> case (varToSPOffset refLeft stack, varToSPOffset refRight stack) of
            (Just lSP, Just rSP) -> add{I.lhs=I.Ref lSP, I.rhs=I.Ref rSP} : next
            _ -> add : next
        (I.Ref refLeft, _) -> case varToSPOffset refLeft stack of
            Just lSP -> add{I.lhs=I.Ref lSP} : next
            _ -> add : next
        (_, I.Ref refRight) -> case varToSPOffset refRight stack of
            Just rSP -> add{I.rhs=I.Ref rSP} : next
            _ -> add : next
        _ -> add : next
    createStackFramesImpl stack (sub@I.Sub{I.lhs=l, I.rhs=r}: xs) = case (l, r) of
        (I.Ref refLeft, I.Ref refRight) -> case (varToSPOffset refLeft stack, varToSPOffset refRight stack) of
            (Just lSP, Just rSP) -> sub{I.lhs=I.Ref lSP, I.rhs=I.Ref rSP} : next
            _ -> sub : next
        (I.Ref refLeft, _) -> case varToSPOffset refLeft stack of
            Just lSP -> sub{I.lhs=I.Ref lSP} : next
            _ -> sub : next
        (_, I.Ref refRight) -> case varToSPOffset refRight stack of
            Just rSP -> sub{I.rhs=I.Ref rSP} : next
            _ -> add : next
        _ -> sub : next


        where next = createStackFrames stack xs




    -- createStackFramesImpl stack (var@I.VariableScope{I.name=n, I.body=b} = (createStackFramesImpl (n:stack) b)




    -- createStackFrames stack 


    --n buildStructure A.IfStatement {} ([FrogNode]) (Maybe FrogNode)
    flattenStructureImpl :: Int -> [ I.Structure ] -> (Int, [ I.Instruction ])

    flattenStructureImpl i ((I.SingleInstruction inst) : xs) = (newI, inst : next)
        where
            (newI, next) = flattenStructureImpl i xs

    flattenStructureImpl i ((I.InstructionSequence seq) : xs) = (newI, seq ++ next)
        where
            (newI, next) = flattenStructureImpl i xs


    flattenStructureImpl i (I.FiniteLoop { I.condition=c, I.body=b} : xs) = (nextI, I.Lbl ("loop_" ++ show i) : bodyInst ++ c ++ I.Jump (I.Label ("loop_" ++ show i)) : nextInst)
        where
            (bodyI, bodyInst) = flattenStructureImpl (i+1) b
            (nextI, nextInst) = flattenStructureImpl bodyI xs

    flattenStructureImpl i (I.InfiniteLoop b: xs) = (nextI, I.Lbl ("loop_" ++ show i) : bodyInst ++ I.Jump (I.Label ("loop_" ++ show i))  : nextInst)
        where
            (bodyI, bodyInst) = flattenStructureImpl (i+1) b
            (nextI, nextInst) = flattenStructureImpl nextI xs

    flattenStructureImpl i (I.Routine{I.name=n, I.body=b}: xs) = (nextI, (I.Lbl n : bInst) ++ [I.RetNone] ++ nextInst)
        where
        (bI, bInst) =  flattenStructureImpl i b
        (nextI, nextInst) = flattenStructureImpl bI xs

    flattenStructureImpl i (I.ConditionalBranch{I.condition=c, I.trueBranch=tb, I.falseBrach=fb} : xs) = case (tb, fb) of
        ([], []) -> (i, [])
        ([], fBranch) -> (falseI + 1, c ++ [ I.BC { I.conditionType=I.Equal, I.target = I.Label ("endif_" ++ show i)}] ++ falseInst ++ [I.Lbl ("endif_" ++ show i)]  )
        (tBranch, []) -> (trueI + 1, c ++ [ I.BC { I.conditionType=I.NotEqual, I.target = I.Label ("endif_" ++ show i)}] ++ trueInst ++ [I.Lbl ("endif_" ++ show i)]  )
        (tBranch, fBranch) -> (falseI + 1, c ++ [ I.BC { I.conditionType=I.NotEqual, I.target = I.Label ("elseif_" ++ show i)}] ++ trueInst ++ [I.Jump (I.Label ("endif_" ++ show i)), I.Lbl ("ifelse" ++ show i)] ++ falseInst ++ [I.Lbl ("endif" ++ show i )] )
        where
            (trueI, trueInst) = flattenStructureImpl (i + 1) tb
            (falseI, falseInst) = flattenStructureImpl trueI fb



    flattenStructureImpl i [] = (i, [])
    flattenStructureImpl i (I.VariableScope {I.assignment=a, I.body=b} : xs) = (i, assignmentInst ++ bodyInst ++ nextInst)
        where
            (assignmenti, assignmentInst) = flattenStructureImpl i a
            (bodyI, bodyInst) = flattenStructureImpl assignmenti b
            (nextI, nextInst) = flattenStructureImpl bodyI xs

    flattenStructureImpl i n = trace ("[flattenStructureImpl] Unknown pattern => " ++ show n) (i, [])


    flattenStructure :: [ I.Structure ] -> [ I.Instruction ]
    flattenStructure struct = res
        where
            (_, res) = flattenStructureImpl 0 struct


