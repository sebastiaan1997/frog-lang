module Executor(execute) where
  import qualified Data.Map as M


  import qualified Ast as A
  import qualified Lexer as L
  import qualified Parser as P
  import Result(Result(..), isSuccess, isUnimplemented, isError, isWarning, mapSuccess, unwrapResult, mapIOSuccess, unwrapIOResult)
  import Debug.Trace (trace, traceShow)
  import Control.Monad(mapM)



  -- Implementation for printing from Froglang
  nativePrint :: Context -> IO (Result (Context, Value))
  nativePrint ctx =
        case str of
          Success Variable{value=StringValue strVal} -> do { print strVal; return (Success (ctx, None)) }
          Success Variable{value=BooleanValue boolVal} -> do { if boolVal then print "true" else print "false"; return (Success (ctx, None)) }
          Success Variable{value=NumValue numVal} -> do { print (show numVal); return (Success (ctx, None)) }
          Warning (Variable{value=StringValue strVal}, msg)-> do { print strVal; return(Warning ((ctx, None),  msg)) }
          Error msg ->  return(Error ("Native call print: " ++  msg))
          Unimplemented res -> return(Unimplemented res)
          otherwise -> return (Error ("Could not find match print for \" " ++ (show str) ++ "\""))
      where str = getVariable ctx "str"
  -- Implementation of the + operator
  nativePlus :: Context -> Result (Context, Value)
  nativePlus ctx = case (lhs, rhs) of
      (Success Variable{value=NumValue lhsNum}, Success Variable{value=NumValue rhsNum}) -> Success(ctx, NumValue (lhsNum + rhsNum))
      (Success Variable{value=IntValue lhsNum}, Success Variable{value=IntValue rhsNum}) -> Success(ctx, IntValue (lhsNum + rhsNum))

      (Success Variable{value=StringValue lhsStr}, Success Variable{value=StringValue rhsStr}) -> Success(ctx, StringValue (lhsStr ++ rhsStr))
    where
      lhs = getVariable ctx "lhs"
      rhs = getVariable ctx "rhs"

  nativePlusPlus :: Context -> Result (Context, Value)
  nativePlusPlus ctx = case lhs of
    (Success Variable{value=NumValue lhsNum}) -> Success(ctx, NumValue (lhsNum + 1))
    (Success Variable{value=IntValue lhsNum}) -> Success(ctx, IntValue (lhsNum + 1))
    where
      lhs = getVariable ctx "lhs"

  -- Implementation of the - operator
  nativeMinus :: Context -> Result (Context, Value)
  nativeMinus ctx = case (lhs, rhs) of
    (Success Variable{value=NumValue lhsNum}, Success Variable{value=NumValue rhsNum}) -> Success(ctx, NumValue (lhsNum - rhsNum))
    (Success Variable{value=IntValue lhsNum}, Success Variable{value=IntValue rhsNum}) -> Success(ctx, IntValue (lhsNum - rhsNum))
    where
      lhs = getVariable ctx "lhs"
      rhs = getVariable ctx "rhs"


  nativeMinusMinus :: Context -> Result (Context, Value)
  nativeMinusMinus ctx = case lhs of
    (Success Variable{value=NumValue lhsNum}) -> Success(ctx, NumValue (lhsNum - 1) )
    (Success Variable{value=IntValue lhsNum}) -> Success(ctx, IntValue (lhsNum - 1) )
    where
      lhs = getVariable ctx "lhs"

  -- Implementation of the * operator
  nativeMultiply :: Context -> Result (Context, Value)
  nativeMultiply ctx = case (lhs, rhs) of
    (Success Variable{value=NumValue lhsNum}, Success Variable{value=NumValue rhsNum}) -> Success(ctx, NumValue (lhsNum * rhsNum))
    (Success Variable{value=IntValue lhsNum}, Success Variable{value=IntValue rhsNum}) -> Success(ctx, IntValue (lhsNum * rhsNum))
    where
      lhs = getVariable ctx "lhs"
      rhs = getVariable ctx "rhs"

  nativeGT :: Context -> Result (Context, Value)
  nativeGT ctx = case (lhs, rhs) of
    (Success Variable{value=NumValue lhsNum}, Success Variable{value=NumValue rhsNum}) -> Success(ctx, BooleanValue (lhsNum > rhsNum))
    (Success Variable{value=IntValue lhsNum}, Success Variable{value=IntValue rhsNum}) -> Success(ctx, BooleanValue (lhsNum > rhsNum))
    where
      lhs = getVariable ctx "lhs"
      rhs = getVariable ctx "rhs"

  nativeLT :: Context -> Result (Context, Value)
  nativeLT ctx = case (lhs, rhs) of
    (Success Variable{value=NumValue lhsNum}, Success Variable{value=NumValue rhsNum}) -> Success(ctx, BooleanValue (lhsNum < rhsNum))
    (Success Variable{value=IntValue lhsNum}, Success Variable{value=IntValue rhsNum}) -> Success(ctx, BooleanValue (lhsNum < rhsNum))
    where
      lhs = getVariable ctx "lhs"
      rhs = getVariable ctx "rhs"


  -- |Implementation of the default native funcitons.
  defaultFunctions :: M.Map String Callable
  defaultFunctions = M.fromList [
    ("print", NativeRoutine{functionName="print", functionParams=[Variable{name="str", typename="String",value=Uninitialized}], targetRoutine=nativePrint})
    ,("++", NativeFunction{functionName="++", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativePlusPlus})
    ,("+", NativeFunction{functionName="+", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativePlus})
    ,("-", NativeFunction{functionName="-", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativeMinus})
    ,("--", NativeFunction{functionName="--", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativeMinus})
    ,("*", NativeFunction{functionName="*", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativeMultiply})
    ,(">", NativeFunction{functionName="*", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativeGT})
    ,("<", NativeFunction{functionName="*", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativeLT})]
    -- ,("", NativeFunction{functionName="+", functionParams=[Variable{name="lhs", typename="",value=Uninitialized}, Variable{name="rhs", typename="",value=Uninitialized}], targetFunction=nativePlus})]

  data Callable = FrogFunction A.FrogNode
    | NativeFunction {functionName :: String, functionParams :: [Variable], targetFunction :: Context -> Result (Context, Value)}
    | NativeRoutine  {functionName :: String, functionParams :: [Variable], targetRoutine :: Context -> IO(Result (Context, Value))}
  instance Show Callable where
    show (FrogFunction node) = "FrogFunction (" ++ show node ++ ")"
    show NativeFunction{functionName=name, functionParams=params} = "NativeFunction {name=" ++ name ++ ", params= " ++ show params ++ ")"
    show NativeRoutine{functionName=name, functionParams=params} = "NativeRoutine {name=" ++ name ++ ", params= " ++ show params ++ ")"
  instance Eq Callable where
    (==) (FrogFunction lhs) (FrogFunction rhs) = lhs == rhs
    (==) NativeFunction{functionName=lhsName, functionParams=lhsParams} NativeFunction{functionName=rhsName, functionParams=rhsParam} = lhsName == rhsName && lhsParams == rhsParam
    (==) _ _ = False
  -- data FunctionValue = Interpeted P.FunctionDefinition | Native ()
  data Value = BooleanValue Bool | StringValue String | IntValue Int |NumValue Float | Function Callable| Exception String | Uninitialized | None
    deriving (Show, Eq)

  getValueTypename :: Value -> String
  getValueTypename (BooleanValue _) = "bool"
  getValueTypename (StringValue _) = "string"
  getValueTypename (NumValue _) = "num"
  getValueTypename (Function _) = "function"
  getValueTypename (Exception _) = "exception"




  data Variable = Variable { name :: String, typename :: String, value:: Value } | Constant { name :: String, typename :: String, value:: Value }
    deriving (Show, Eq)

  data Struct = Struct { structName :: String, fields :: M.Map String Variable, assocFunctions :: M.Map String Callable}
    deriving (Show, Eq)

  data Context = Context { 
    scope :: String, 
    structs :: M.Map String Struct, 
    variables :: M.Map String Variable, 
    functions :: M.Map String Callable, 
    parent :: Maybe Context, 
    returnContext :: Maybe Context, 
    rootContext:: Maybe Context
    }
    deriving (Show, Eq)

  evaluateToBoolean :: Value -> Result Bool
  evaluateToBoolean (BooleanValue v) = Success v
  evaluateToBoolean (NumValue num) = Warning(num /= 0, "Numtype to boolean conversion, can be unintended")
  evaluateToBoolean value = Error ("Cannot convert type to boolean, value: " ++ show value)

  getVariable :: Context -> String -> Result Variable
  getVariable _ "" = Error "Empty variable name"
  getVariable ctx name
    | M.notMember name  (variables ctx) = case parent ctx of
      Just p -> getVariable p name
      Nothing -> Error ("Could not find variable " ++ name ++ " in context")
    | otherwise  = case M.lookup name (variables ctx) of
      Just var -> Success var
      _ -> Error ("Could not find variable " ++ name ++ " in context")

  createVariable :: Context -> String -> String -> Value -> Result Context
  createVariable ctx name typename value = Success ctx{variables=newMap}
    where newMap = M.insert name Variable{name=name, typename="", value=value} (variables ctx)

  createConstant :: Context -> String -> String -> Value -> Result Context
  createConstant ctx name typename value = Success ctx{variables=newMap}
    where newMap = M.insert name Constant{name=name, typename="", value=value} (variables ctx)


  updateVariable :: Context -> String -> Value -> Result Context
  updateVariable ctx name newValue
    | M.notMember name (variables ctx) = case parent ctx of
      Just p -> (case updateVariable p name newValue of Success newP -> Success ctx{parent=Just newP}; Error e -> Error e)
      Nothing -> Error ("Could not update variable " ++ name ++ " because it does not excist")
    | otherwise = Success(ctx{variables=M.update (\var -> Just var{value=newValue}) name (variables ctx)})

  registerFunction :: Context -> A.FrogNode -> Result (Context, Value)
  registerFunction ctx A.RoutineDefinition{A.name = name, A.params = params, A.body = body}
    | M.notMember name (functions ctx) = Error "Not implemented"
    | otherwise = Error ("Function " ++ name ++ "already defined, cannot define multiple times")


  evaluateSequence :: Context -> [A.FrogNode] -> IO (Result (Context, Value))
  evaluateSequence ctx [n] = evaluate ctx n
  evaluateSequence ctx (x:xs) = do {
      result <- evaluate ctx x;
      unwrapIOResult (mapIOSuccess (\c -> evaluate (fst c) (A.Sequence xs)) result)
    }


  evaluate :: Context -> A.FrogNode -> IO (Result (Context, Value))
  -- evaluate ctx (P.Sequence (P.FunctionReturn node : xs)) = evaluatedReturn >>= (\ret -> return (Success(ctx, ret)))
  --  where evaluatedReturn = evaluate ctx node*
  evaluate ctx A.ConstDeclaration{A.name=n, A.assignment=a, A.typename=t} = newContext >>= (return . mapSuccess (\y -> (y, None)))
    where
      assignmentResult = evaluate ctx a
      newContext = assignmentResult >>= (return . unwrapResult . mapSuccess (\y -> createConstant (fst y) n t (snd y)))



  evaluate ctx (A.Sequence seq) = evaluateSequence ctx seq
  evaluate ctx A.VarDeclaration{A.name=n, A.assignment=a, A.typename=t} = newContext >>= (return . mapSuccess (\y -> (y, None)))
      where
        assignmentResult = evaluate ctx a
        newContext = assignmentResult >>= (return . unwrapResult . mapSuccess (\y -> createVariable (fst y) n t (snd y)))


  evaluate ctx A.ConstDeclaration{A.name=n, A.assignment=a, A.typename=""} = newContext >>= (return . mapSuccess (\y -> (y, None)))
      where
        assignmentResult = evaluate ctx a
        newContext = assignmentResult >>= (return . unwrapResult . mapSuccess (\y -> createConstant (fst y) n "" (snd y)))

  evaluate ctx A.VarDeclaration{A.name=n, A.assignment=a, A.typename=""} = newContext >>= (return . mapSuccess (\y -> (y, None)))
        where
          assignmentResult = evaluate ctx a
          newContext = assignmentResult >>= (return . unwrapResult . mapSuccess (\y -> createVariable (fst y) n "" (snd y)))




  evaluate ctx A.Assignment{A.target=name, A.assignment=assignment} = newContext >>= (return . mapSuccess (\y -> (y, None)))
      where
        assignmentResult = evaluate ctx assignment
        newContext = assignmentResult >>= (return . unwrapResult . mapSuccess (\y -> updateVariable (fst y) name (snd y)))

  evaluate ctx (A.Token(L.Identifier name)) = return (mapSuccess (\x -> (ctx, value x)) variable)
      where variable = getVariable ctx name
  evaluate ctx (A.Variable name) = return (mapSuccess (\x -> (ctx, value x)) variable)
    where variable = getVariable ctx name


  evaluate ctx A.FunctionCall {A.target=target, A.params=targetArgs} = functionCall ctx target targetArgs
  evaluate ctx (A.Token (L.DigitLiteral digit)) = return(Success (ctx, NumValue digit))
  evaluate ctx (A.Token (L.StringLiteral string)) = return(Success (ctx, StringValue string))
  evaluate ctx A.RoutineDefinition {A.name = name, A.params = params, A.body = body} = return(registerFunction ctx A.RoutineDefinition {A.name = name, A.params = params, A.body = body})
  evaluate ctx A.FunctionDefinition {A.name = name, A.params = params, A.body = body} = return (registerFunction ctx A.FunctionDefinition {A.name = name, A.params = params, A.body = body})
  evaluate ctx (A.Token(L.StringLiteral val)) = return(Success (ctx, StringValue val))
  evaluate ctx (A.Token(L.DigitLiteral val)) = return(Success (ctx, NumValue val))
  evaluate ctx A.Block{A.open='{', A.childeren=body, A.close='}'} = evaluate ctx (A.Sequence body)
  evaluate ctx (A.Statement stmt) = evaluate ctx stmt

  evaluate ctx(A.Token(L.Keyword L.TrueVal)) = return (Success (ctx, BooleanValue True))
  evaluate ctx(A.Token(L.Keyword L.FalseVal)) = return (Success (ctx, BooleanValue False))
  evaluate ctx (A.Statement(A.Sequence [n])) = evaluate ctx n
  evaluate ctx A.IfStatement{A.condition=c, A.body=b} = do {
      conditionValue <- conditionValueResult;
      unwrapIOResult (unwrapIOResult (mapIOSuccess (\x -> if x then bodyResult else elseResult) conditionValue))
    }
    where
      conditionResult = evaluate ctx c
      conditionValueResult = conditionResult >>= (return . unwrapResult . mapSuccess (\y -> evaluateToBoolean (snd y)))
      bodyResult = conditionResult >>= mapIOSuccess (\y -> evaluate (fst y) (A.Sequence b))
      elseResult = conditionResult >>= mapIOSuccess (\y -> return (Success (fst y, None)))


  evaluate ctx A.WhileStatement{A.condition=c, A.body=b} = do {
        conditionValue <- conditionValueResult;
        bodyRes <- bodyResult;
        unwrapIOResult (unwrapIOResult (mapIOSuccess (\x ->  mapIOSuccess(\y -> if x then evaluate (fst y) nextNode else return (Success (ctx, None))) bodyRes) conditionValue))
      }
      where
        conditionResult = evaluate ctx c
        conditionValueResult = conditionResult >>= (return . unwrapResult . mapSuccess (\y -> evaluateToBoolean (snd y)))
        bodyResult = conditionResult >>= (unwrapIOResult . mapIOSuccess (\y -> evaluateSequence (fst y) b))
        nextNode = A.WhileStatement{A.condition=c, A.body=b}



  evaluate ctx (A.Sequence []) = return (Success (ctx, None))
  evaluate ctx (A.FunctionReturn Nothing) =  return (Success (ctx, None))

  evaluate ctx (A.FunctionReturn (Just functionReturn)) = returnResult >>= (return . mapSuccess (\value -> value))
    where returnResult = evaluate ctx functionReturn
  evaluate ctx A.InfixCall{A.lhs=lhs, A.target=f, A.rhs=rhs} = functionCall ctx f [lhs, rhs]
  evaluate ctx A.SelfAssigningInfixCall{A.lhs=A.Variable(name), A.target=f, A.rhs=rhs} = do {
      result <- functionCall ctx f [A.Variable name, rhs];
      return (unwrapResult(mapSuccess (\r -> mapSuccess (\c -> (c, snd r)) (updateVariable ctx name (snd r))) result))
    }


  evaluate ctx node = return(Error ("Cannot process node: " ++ show node))


  mergeContexts :: Context -> Context -> Context
  mergeContexts lhs rhs = lhs{variables=vars, functions=funcs, structs=structDefs}-- Context{scope=(scope lhs), variables=vars, functions=funcs, parent=(parent lhs), returnContext=(returnContext lhs), rootContext=(rootContext lhs)}
    where
      vars = M.union (variables lhs)(variables rhs)
      funcs = M.union (functions lhs)(functions rhs)
      structDefs = M.union (structs lhs)(structs rhs)

  emptyContext :: Context
  emptyContext = Context {scope="", structs=M.empty, variables=M.empty, functions= M.empty, parent=Nothing, returnContext=Nothing, rootContext=Nothing}
  createContext :: A.FrogNode -> Result Context
  createContext (A.Statement stmt) = createContext stmt
  createContext A.FunctionDefinition{A.name=name, A.params=params, A.body=args} = Success emptyContext{scope=name, functions= functionCollection}
    where
      definition = A.FunctionDefinition{A.name=name, A.params=params, A.body=args}
      functionCollection =  M.union (M.fromList [(name, FrogFunction definition)])  defaultFunctions
  createContext A.RoutineDefinition{A.name=name, A.params=params, A.body=args} = Success emptyContext {scope=name,functions=functionCollection}
      where
        definition = A.RoutineDefinition{A.name=name, A.params=params,A.body=args}
        functionCollection =  M.union(M.fromList [(name, FrogFunction definition)])  defaultFunctions
  createContext (A.Sequence children) = foldl1 (\ lhs rhs -> unwrapResult( mapSuccess (\l -> mapSuccess (mergeContexts l) rhs) lhs )) ctxs
    where ctxs = map createContext children

  --createContext (P.StructDefinition{P.name=n,P.fields=f}) = Success emptyContext{structs=M.fromList [(n, Struct{structName=n, fields=M.fromList fieldMap, assocFunctions=M.empty})]}
  --  where fieldMap = map (\x -> (P.name x, Variable{name=P.name x, typename=P.typename x, value=Uninitialized} )) f

  createContext node = Error ("Could not index element: " ++ show node)

  fromArgList :: [(String, Value)] -> [(String, Variable)]
  fromArgList = map  ( \(name, value) -> (name, Variable{name=name, typename="", value=value}))

  getFunction :: Context -> String -> [A.FrogNode] -> Result Callable

  getFunction Context {functions=functions, parent=Nothing, scope=scopeName, rootContext=Nothing} name args = case M.lookup name functions of
      Just func -> Success func
      Nothing -> Error ("Function `" ++ name ++  "` not found in context scope " ++ scopeName)

  getFunction Context {functions=functions, parent=Nothing, scope=scopeName, rootContext=Just root} name args = case M.lookup name functions of
    Just func -> Success func
    Nothing -> getFunction root name args
  getFunction ctx name args = case M.lookup name (functions ctx) of
    Just func -> Success func
    Nothing -> getFunction ctx name args



  bindArgs :: Context -> Callable -> [A.FrogNode] -> IO (Result (M.Map String Variable))
  bindArgs _ (FrogFunction A.FunctionDefinition{A.params=[]}) [] = return (Success M.empty)
  bindArgs _ (FrogFunction A.RoutineDefinition{A.params=[]}) [] = return (Success M.empty)
  -- bindArgs ctx (P.FunctionDefinition{P.parent=parent, P.params=P.Sequence params, P.name=name}) args = map (\(param, arg) ->if (P.Ar))(zip  args)
  bindArgs _ (FrogFunction A.FunctionDefinition{A.name=fnName, A.params=params}) args = return (Unimplemented (Just ("Function Definition " ++ fnName)))
  bindArgs _ (FrogFunction A.RoutineDefinition{A.name=rtName, A.params=params}) args = return(Unimplemented (Just ("Routine Definition " ++ rtName)))
--  data Callable = FrogFunction P.FrogNode | NativeFunction {functionName :: String, functionParams :: [Variable], targetFunction :: Context -> Result (Context, Value)}
  bindArgs Context{ returnContext=Just rctx } NativeFunction{functionParams=funcParams} args = do {
    Success . M.fromList <$> mappedArgs;
    }
    where
      evaluatedArgs = mapM (evaluate rctx) args
      combinedArgs = evaluatedArgs >>= (return . zip funcParams)
      mappedArgs = combinedArgs >>= (return . map (\(param, arg) -> case arg of Success (_, val) -> ((name param), param{value=val});Warning ((_, val), _) -> ((name param), param{value=val});otherwise->((name param), param)))


  bindArgs Context{ returnContext=Just rctx }  NativeRoutine{functionParams=funcParams} args = do {
      Success . M.fromList <$> mappedArgs;
     }
     where
       evaluatedArgs = mapM (evaluate rctx) args
       combinedArgs = evaluatedArgs >>= (return . zip funcParams)
       mappedArgs = combinedArgs >>= (return . map (\(param, arg) -> case arg of Success (_, val) -> ((name param), param{value=val});Warning ((_, val), _) -> ((name param), param{value=val});otherwise->((name param), param)))

  functionCall :: Context -> String -> [A.FrogNode] -> IO (Result (Context, Value))
  functionCall ctx name args = do{
    ctxRes <- newCtxResult;
    result <- (case function of
        (Success (FrogFunction A.FunctionDefinition{A.body=body})) -> unwrapIOResult (mapIOSuccess (`evaluateSequence` body) ctxRes)
        (Warning (FrogFunction A.FunctionDefinition{A.body=body}, _)) -> unwrapIOResult (mapIOSuccess (`evaluateSequence` body) ctxRes)
        (Success (FrogFunction A.RoutineDefinition{A.body=body})) -> unwrapIOResult (mapIOSuccess (`evaluateSequence` body) ctxRes)
        (Warning (FrogFunction A.RoutineDefinition{A.body=body}, _)) -> unwrapIOResult (mapIOSuccess (`evaluateSequence` body) ctxRes)
        (Error msg) -> return (Error msg)
        (Success NativeFunction{targetFunction=func}) -> return(unwrapResult (mapSuccess func ctxRes))
        (Success NativeRoutine{targetRoutine=rt}) -> unwrapIOResult( mapIOSuccess rt ctxRes )
        (Unimplemented msg) -> return (Unimplemented msg)
        otherwise -> return(Unimplemented (Just "Function call not implemented for partern")));

    return (case result of
      Success (Context{returnContext=Just rctx}, v) -> Success (rctx, v)
      Warning ((Context{returnContext=Just rctx}, v), msg) -> Warning((rctx, v), msg)
      Error msg -> Error msg
      Unimplemented msg -> Unimplemented msg
      otherwise -> Error "Return context not available in scope "
    )
  }
    where
      function      = getFunction ctx name args
      childCtx      = emptyContext {scope=name, variables=M.empty, functions=defaultFunctions, parent=Nothing, returnContext=Just ctx, rootContext=rootContext ctx}
      arguments     = unwrapIOResult (mapIOSuccess (\f -> bindArgs childCtx f args) function)
      newCtxResult  = arguments >>= mapIOSuccess (\var -> return childCtx{variables=var})


  execute :: A.FrogNode -> IO()
  execute node = do {
      print "Abstract source tree:";
      print (show node);
      print "Start execution...";
      print "====";
      -- print (ctxResult);
      result <- unwrapIOResult (mapIOSuccess (\ctx -> functionCall ctx{rootContext=Just ctx} "main" []) ctxResult);
      case result of
        Warning(_, "") -> print "Warning: Executed with warnings, no message given."
        Warning(_, msg) -> print ("Warning: " ++  msg)
        Error "" -> print "Warning: Executed with warnings, no message given."
        Error msg -> print ("Error: " ++ msg ++ "\n")
        Unimplemented(Just msg) -> print ("Unimplemented: " ++ msg)
        Unimplemented Nothing -> print "Unimplemented: no message given"
        _ -> return ()
      -- print(result)
    }
    where ctxResult = createContext node






