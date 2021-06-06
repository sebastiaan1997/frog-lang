{-# LANGUAGE LambdaCase #-}

module IlCompiler(compile, compileOptimize, buildStructure) where
    import qualified Ast as A
    import qualified Il as I
    import qualified Cpu as C
    import qualified IlCpuOptimizer as CO
    import qualified IlOptimizer as O
    import qualified Data.Map as M

    import Debug.Trace (trace, traceShow, traceShowId)
    import Data.Maybe (fromMaybe, fromJust, catMaybes, mapMaybe, maybeToList)
    import Data.List (elemIndex)

    -- | StructureInfo
    data StructureInfo = StructureInfo { exports :: [String], ro :: M.Map String I.Data, rw :: M.Map String I.Data, structure :: [I.Structure] }
        deriving (Show)
    mergeStructureInfo :: StructureInfo -> StructureInfo -> StructureInfo
    mergeStructureInfo StructureInfo{exports=lhsExports, ro=lhsRo, rw=lhsRw, structure=lhsStructure} StructureInfo{exports=rhsExports, ro=rhsRo, rw=rhsRw, structure=rhsStructure}
        = StructureInfo{exports=lhsExports++rhsExports, ro=M.union lhsRo rhsRo, rw=M.union lhsRw rhsRw, structure=lhsStructure ++ rhsStructure}

    -- Defines default empty structure info.
    emptyStructureInfo :: StructureInfo
    emptyStructureInfo = StructureInfo{
        exports=[],
        ro=M.empty,
        rw=M.empty,
        structure=[]
    }

    -- | Compiles AST to IL Document
    compile :: A.FrogNode -> I.ILDocument
    compile n  =  ((foldConditions) (flattenStructure  (buildStructure [n])))
    -- | Compiles AST and optimize for target to IL Document
    compileOptimize :: C.Cpu -> A.FrogNode -> I.ILDocument
    compileOptimize c n = (CO.fillRegisters c . O.optimizeStack . foldConditions . flattenStructure . buildStructure) [n]





    -- prepareCondition ::  A.FrogNode  -> I.Instruction
    convertData :: A.FrogNode -> Maybe I.Data
    convertData (A.BooleanLiteral val) = Just (I.BooleanValue val)
    convertData (A.NumericLiteral f) =  Just (I.IntegerValue (round f))
    convertData (A.Variable v) = Just (I.Ref (I.Variable v))
    convertData _ = Nothing







    compileAssignment :: I.Reference -> A.FrogNode -> Maybe [ I.Instruction ]
    -- compileAssignment "" _ = Nothing
    compileAssignment var c@A.FunctionCall{A.target=fn, A.params=[]} = Just [ I.Call{I.routine=I.Label fn, I.args=[], I.storeReturn=Just var}]
    compileAssignment var c@A.FunctionCall{A.target=fn, A.params=p} = Just [ I.Call{I.routine=I.Label fn, I.args=mapMaybe convertData p, I.storeReturn=Just var}]

    compileAssignment var (A.BooleanLiteral b) = Just [ I.PushVar { I.store=var, I.source = I.BooleanValue b} ]
    compileAssignment var (A.NumericLiteral f) = Just [ I.PushVar { I.store=var, I.source = I.IntegerValue (round f)} ]
    compileAssignment var (A.IntegerLiteral i) = Just [ I.PushVar { I.store=var, I.source = I.IntegerValue i }]
    compileAssignment var (A.Variable v) = Just [ I.PushVar { I.store=var, I.source = I.Ref (I.Variable v)}]
    compileAssignment var infx@A.InfixCall {A.lhs=l, A.target=t, A.rhs=r}  = traceShow infx (case (t, convertData l, convertData r, next) of
            ("+", Just lVal, Just rVal, Nothing) -> Just [ I.Add { I.store=var, I.lhs=lVal, I.rhs=rVal}]
            ("+", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Add { I.store= var, I.lhs=lVal, I.rhs=I.Ref var }])
            ("-", Just lVal, Just rVal, Nothing) -> Just [ I.Sub { I.store=var, I.lhs=lVal, I.rhs=rVal}]
            ("-", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Sub { I.store=var, I.lhs=lVal, I.rhs=I.Ref var }])
            ("*", Just lVal, Just rVal, Nothing) -> Just [ I.Mul { I.store=var, I.lhs=lVal, I.rhs=rVal}]
            ("*", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Mul { I.store=var, I.lhs=lVal, I.rhs=I.Ref var }])
            ("/", Just lVal, Just rVal, Nothing) -> Just [ I.Div { I.store= var, I.lhs=lVal, I.rhs=rVal}]
            ("/", Just lVal, Nothing, Just n)    -> Just  (n ++ [I.Div { I.store= var, I.lhs=lVal, I.rhs=I.Ref var }])
            b -> trace ("[compileAssignment] failed" ++ show b) Nothing)
        where
            next = compileAssignment var r



    compileAssignment var node = trace ("[compileAssignment] Unknown pattern for variable= " ++ show var ++ " => " ++ show node) Nothing

    compileIfCondition :: A.FrogNode -> Maybe [I.Instruction]
    compileIfCondition (A.Variable var) = Just [ I.Cmp {I.lhs=I.Ref (I.Variable var), I.rhs=I.BooleanValue True} ]
    compileIfCondition A.InfixCall{A.lhs=A.Variable l, A.target=t, A.rhs=A.IntegerLiteral r} = Just [ I.CmpStmtNext {I.lhs=I.Ref (I.Variable l), I.rhs=I.IntegerValue r, I.conditionType=conditionType} ]
        where
            conditionType = I.invertCondition (case t of
                "==" -> I.Equal
                "!=" -> I.NotEqual
                ">"  -> I.Greater
                "<"  -> I.Lesser
                ">="  -> I.GreaterEqual
                "<="  -> I.LesserEqual)

    compileIfCondition _ = Nothing






    buildStructure :: [A.FrogNode]  -> StructureInfo
    -- |Builds a structure in with IL to represent the AST in IL
    buildStructure = foldr buildStructureImpl emptyStructureInfo

    buildStructureImpl :: A.FrogNode -> StructureInfo -> StructureInfo
    -- | Unwraps a statement
    buildStructureImpl (A.Statement stmt) info =  buildStructureImpl stmt info
    -- | 
    buildStructureImpl (A.Assignment{ A.target=t, A.assignment=a }) info@StructureInfo{structure=s} = case compileAssignment (I.Variable t) a of
        Just instructions -> info{structure=I.InstructionSequence instructions : s}
        _ -> info


    buildStructureImpl  A.InfixCall{A.lhs=l, A.target="+", A.rhs=r} info@StructureInfo{structure=s} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> info{structure=I.SingleInstruction I.Add{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR} : s}
        (Just dataL, Nothing) ->  mergeStructureInfo info{structure=I.SingleInstruction I.Add{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : (structure rinst)  ++ s} rinst{structure=[]}
        where
            rinst = buildStructure [r]


    buildStructureImpl  A.InfixCall {A.lhs=l, A.target="-", A.rhs=r} info@StructureInfo{structure=s} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> info{structure=I.SingleInstruction I.Sub{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR} : s}
        (Just dataL, Nothing) ->  mergeStructureInfo info{structure=I.SingleInstruction I.Sub{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : (structure rinst)  ++ s} rinst{structure=[]}
        where
            rinst = buildStructure [r]

    buildStructureImpl  A.InfixCall {A.lhs=l, A.target="*", A.rhs=r} info@StructureInfo{structure=s} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> info{structure=I.SingleInstruction I.Mul{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR}: s}
        (Just dataL, Nothing) ->  mergeStructureInfo info{structure=I.SingleInstruction I.Mul{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : (structure rinst)  ++ s} rinst{structure=[]}
        where
            rinst = buildStructure [r]


    buildStructureImpl  A.InfixCall {A.lhs=l, A.target="/", A.rhs=r} info@StructureInfo{structure=s} = case (convertData l, convertData r) of
        (Just dataL, Just dataR) -> info{structure=I.SingleInstruction I.Div{I.store=I.Prev, I.lhs=dataL, I.rhs=dataR}:s}
        (Just dataL, Nothing) ->  mergeStructureInfo info{structure=I.SingleInstruction I.Div{I.store=I.Prev, I.lhs=dataL, I.rhs=I.Ref I.Next} : (structure rinst)  ++ s} rinst{structure=[]}
        where
            rinst = buildStructure [r]

    -- buildStructureImpl A.Increment { A.lhs=A.Variable var } = [ I.SingleInstruction (I.Increment (I.Variable var)) ]
    -- buildStructureImpl A.Decrement { A.lhs=A.Variable var } = [ I.SingleInstruction (I.Decrement (I.Variable var)) ] 

    buildStructureImpl A.Block { A.childeren=c } info@StructureInfo{structure=s} = mergeStructureInfo info{structure=(I.Scope . structure) b : s} b{structure=[]}
        where
            b = buildStructure c

    buildStructureImpl A.Increment { A.lhs=A.Variable var} info@StructureInfo{structure=s}
        = info{structure= I.SingleInstruction (I.Increment (I.Variable var)) : s}
    buildStructureImpl A.Decrement { A.lhs=A.Variable var} info@StructureInfo{structure=s}
        = info{structure= I.SingleInstruction (I.Decrement (I.Variable var)) : s}

    buildStructureImpl A.SelfAssigningInfixCall { A.lhs=A.Variable var, A.target = t,  A.rhs = r }  info@StructureInfo{structure=s}
        = case (convertData r, t) of
            (Just op, "+=") -> info{structure=I.SingleInstruction (I.Add{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}):s}
            (Just op, "-=") -> info{structure=I.SingleInstruction (I.Sub{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}):s}
            (Just op, "*=") -> info{structure=I.SingleInstruction (I.Mul{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}):s}
            (Just op, "/=") -> info{structure=I.SingleInstruction (I.Div{I.store=I.Variable var, I.lhs=I.Ref (I.Variable var), I.rhs=op}):s}
            (_, _) -> info
    buildStructureImpl (A.FunctionReturn Nothing) info@StructureInfo{structure=s}  = info{structure=I.SingleInstruction I.RetNone:s}


    buildStructureImpl (A.FunctionReturn (Just c@A.FunctionCall{})) info@StructureInfo{structure=s} = case compileAssignment I.ReturnValue c of
        Just instr -> info{structure=I.InstructionSequence instr:s}
        _ -> info

    buildStructureImpl (A.FunctionReturn (Just val)) info@StructureInfo{structure=s} = case convertData val of
        Just value -> info{structure = I.SingleInstruction (I.Ret value):s}
        _ -> info{structure=I.SingleInstruction I.RetNone:s}


    buildStructureImpl (A.Sequence (A.VarDeclaration{A.name=n, A.assignment=a, A.typename=t}: xs)) info@StructureInfo{structure=s}
        = mergeStructureInfo current next
        where
            current = info{structure=I.VariableScope{I.name=n, I.typename=t, I.assignment=[I.InstructionSequence (fromMaybe [] (compileAssignment (I.Variable n) a))], I.body=structure next} : s}
            next = buildStructure  xs

    buildStructureImpl (A.Sequence (A.ConstDeclaration{A.name=n, A.assignment=a, A.typename=t}: xs)) info@StructureInfo{structure=s}
        = mergeStructureInfo current next
        where
            current = info{structure=I.ConstantScope{I.name=n, I.typename=t, I.assignment=[I.InstructionSequence (fromMaybe [] (compileAssignment (I.Variable n) a))], I.body=structure next} : s}
            next = buildStructure  xs

    buildStructureImpl (A.Sequence seq)  info@StructureInfo{structure=s} = mergeStructureInfo info (buildStructure seq)


    buildStructureImpl A.RoutineDefinition {A.name=n, A.params=p, A.body=b} info@StructureInfo{exports=e,structure=s, ro=roValue, rw=rwValue}
        = mergeStructureInfo info{exports= n : e, structure=I.Routine { I.name=n, I.params=paramNames, I.body= structure bodyResult}:s} bodyResult{structure=[]}
            where
                paramData =  mapMaybe convertData p
                paramNames = mapMaybe (\case (I.Ref (I.Variable var)) -> Just var; _ -> Nothing) paramData
                bodyResult = buildStructure b

    buildStructureImpl A.FunctionDefinition {A.name=n, A.params=p, A.body=b} info@StructureInfo{exports=e,structure=s, ro=roValue, rw=rwValue}
        = mergeStructureInfo info{exports=n:e, structure=I.Routine { I.name=n, I.params=paramNames, I.body= structure bodyResult} : s} bodyResult{structure=[]}
            where
                paramData =  mapMaybe convertData p
                paramNames = mapMaybe (\case (I.Ref (I.Variable var)) -> Just var; _ -> Nothing) paramData
                bodyResult = buildStructure b



    buildStructureImpl A.FunctionCall{ A.target=funcName, A.params=[]} info@StructureInfo{exports=e,structure=s, ro=roValue, rw=rwValue}
        = info{structure=I.SingleInstruction I.Call{ I.storeReturn = Nothing, I.routine=I.Label funcName, I.args=[]} : s}




    buildStructureImpl A.IfStatement { A.condition = c, A.body=body, A.next= n} info@StructureInfo{exports=e,structure=s, ro=roValue, rw=rwValue} =
        case(c, n, ci) of
            (A.Variable v, Just next, _) -> mergeStructureInfo info{structure=I.ConditionalBranch { I.condition=[I.SingleInstruction (I.Cmp { I.lhs=I.Ref (I.Variable v), I.rhs = I.BooleanValue True})], I.trueBranch=structure bInst, I.falseBranch=structure nInst } : s} nInst{structure=[]}
            (A.Variable v, Nothing, _) -> info{structure=I.ConditionalBranch { I.condition=[I.SingleInstruction I.Cmp { I.lhs=I.Ref (I.Variable v), I.rhs = I.BooleanValue True}], I.trueBranch=structure bInst, I.falseBranch=[] } : s}
            (A.BooleanLiteral True, Just next, _) -> mergeStructureInfo info bInst
            (A.BooleanLiteral False, Just next, _) -> mergeStructureInfo info nInst
            (A.BooleanLiteral False, Nothing, _) -> info
            (_, Nothing, cinst) ->  mergeStructureInfo info{structure=I.ConditionalBranch { I.condition=cinst, I.trueBranch=structure bInst, I.falseBranch=[] } : s} bInst{structure=[]}
            (_, _, cinst) ->  (mergeStructureInfo nInst{structure=[]} . mergeStructureInfo bInst{structure=[]}) info{structure=I.ConditionalBranch { I.condition=cinst, I.trueBranch=structure bInst, I.falseBranch= structure nInst } : s}

        where
            ci = case compileIfCondition c of
                Just [i] -> [I.SingleInstruction i]
                Just ins -> [I.InstructionSequence ins]
                _ -> []

            nInst = buildStructure (maybeToList  n)
            bInst = buildStructure  body

    buildStructureImpl A.WhileStatement { A.condition=A.BooleanLiteral True, A.body=b} info@StructureInfo{exports=e,structure=s, ro=roValue, rw=rwValue}
        = mergeStructureInfo info{structure=I.InfiniteLoop{ I.body=structure body} : s} body{structure=[]}
        where
            body = buildStructure b


    buildStructureImpl A.WhileStatement { A.condition=c, A.body=b} info@StructureInfo{exports=e,structure=s, ro=roValue, rw=rwValue}
        = case compileIfCondition c of
            Just [cInst] -> mergeStructureInfo info{structure=I.FiniteLoop {I.condition=[I.SingleInstruction cInst], I.body=structure bodyInfo}: s} bodyInfo{structure=[]}
            Just [] -> mergeStructureInfo info{structure=I.InfiniteLoop { I.body=structure bodyInfo } : s} bodyInfo{structure=[]}
            Just cInst -> mergeStructureInfo info{structure=I.FiniteLoop {I.condition=[I.InstructionSequence cInst], I.body=structure bodyInfo }: s} bodyInfo{structure=[]}
            _ -> info
        where
            bodyInfo = buildStructure b

    buildStructureImpl A.Nop info@StructureInfo{} = info
    buildStructureImpl n info = trace ("[buildStructureImpl] failed ->" ++ show n) info

    getSPOffset :: String -> [ String ] -> Maybe Int
    getSPOffset = elemIndex

    varToSPOffset :: I.Reference -> [ String ] -> Maybe I.Reference
    varToSPOffset (I.Variable var) stack = case getSPOffset var stack of
        Just off -> Just (I.StackPointerOffset off)
        Nothing -> Nothing
    varToSPOffset ref _ = Just ref



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

    flattenStructureImpl i (I.Routine{I.name=n, I.body=b, I.params=p}: xs) = (nextI, (I.Lbl n : I.RoutineInit (map I.Variable p) : bInst) ++ ([I.RetNone | not isClosed]) ++ nextInst)
        where
        (bI, bInst) =  flattenStructureImpl i b
        isClosed = case last bInst of I.RetNone -> True; I.Ret _ -> True; I.Call{I.storeReturn=Just I.ReturnValue} -> True; _ -> False
        (nextI, nextInst) = flattenStructureImpl bI xs
    -- |
    flattenStructureImpl i (I.ConditionalBranch{I.condition=c, I.trueBranch=tb, I.falseBranch=fb} : xs) = case (tb, fb) of
        ([], []) -> (i, [])
        ([], fBranch) -> (nextI + 1, condtionInstr ++ [ I.BC { I.conditionType=I.Equal, I.target = I.Label ("endif_" ++ show i)}] ++ falseInst ++ [I.Lbl ("endif_" ++ show i)] ++ nextInst  )
        (tBranch, []) -> (nextI + 1, condtionInstr ++ [ I.BC { I.conditionType=I.NotEqual, I.target = I.Label ("endif_" ++ show i)}] ++ trueInst ++ [I.Lbl ("endif_" ++ show i)] ++ nextInst )
        (tBranch, fBranch) -> (nextI + 1, condtionInstr ++ [ I.BC { I.conditionType=I.NotEqual, I.target = I.Label ("elseif_" ++ show i)}] ++ trueInst ++ [I.Jump (I.Label ("endif_" ++ show i)), I.Lbl ("ifelse" ++ show i)] ++ falseInst ++ [I.Lbl ("endif" ++ show i )]  ++ nextInst)
        where
            (conditionI, condtionInstr) = flattenStructureImpl (i + 1) c
            (trueI, trueInst) = flattenStructureImpl conditionI tb
            (falseI, falseInst) = flattenStructureImpl trueI fb
            (nextI, nextInst) = flattenStructureImpl falseI xs
    
    flattenStructureImpl i [] = (i, [])
    flattenStructureImpl i (I.VariableScope {I.name=n, I.assignment=a, I.body=b} : xs) = trace ("[flattenStructureImpl, VariableScope.assignment] " ++ show assignmentInst) (i, assignmentInst ++ bodyInst ++ nextInst ++ [I.PopVar [I.Variable n]])
        where
            (assignmenti, assignmentInst) = flattenStructureImpl i a
            (bodyI, bodyInst) = flattenStructureImpl assignmenti b
            (nextI, nextInst) = flattenStructureImpl bodyI xs

    flattenStructureImpl i n = trace ("[flattenStructureImpl] Unknown pattern => " ++ show n) (i, [])


    -- | 
    flattenStructure :: StructureInfo -> I.ILDocument
    flattenStructure StructureInfo{exports=e, ro=roData, rw=rwData , structure=s} = I.ILDocument{
        I.exports=e,
        I.ro=roData,
        I.rw=rwData,
        I.instructions= (I.InstructionList . snd . flattenStructureImpl 0) s
    }

    -- | Fold conditions 
    foldConditions :: I.ILDocument  -> I.ILDocument
    foldConditions doc@I.ILDocument{I.instructions=I.InstructionList i} = doc{I.instructions= I.InstructionList (foldr foldRConditions [] i)}


    foldRConditions :: I.Instruction -> [I.Instruction] -> [I.Instruction]

    foldRConditions I.CmpStmtNext{I.lhs=l, I.rhs=r, I.conditionType=cType} (b@I.B{} : xs) = I.Cmp{I.lhs=l, I.rhs=r} : b{I.conditionType=cType} : xs
    foldRConditions I.CmpStmtNext{I.lhs=l, I.rhs=r, I.conditionType=cType} (b@I.BC{} : xs) = I.Cmp{I.lhs=l, I.rhs=r} : b{I.conditionType=cType} : xs
    foldRConditions I.CmpStmtNext{I.lhs=l, I.rhs=r, I.conditionType=cType} ((I.Jump jumpTarget) : xs) = I.Cmp{I.lhs=l, I.rhs=r} : I.BC{I.conditionType=cType, I.target=jumpTarget} : xs


    foldRConditions lhs rhs = lhs:rhs








    -- prepareForCpu :: [I.Structure] -> C.Cpu -> [I.Structure]
    -- prepareForCpu 





