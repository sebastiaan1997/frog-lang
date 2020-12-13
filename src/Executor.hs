module Executor(execute) where
  import qualified Data.Map as M

  import qualified Lexer as L
  import qualified Parser as P

  data Value = StringValue String | NumValue Float | Function P.FrogNode | Exception String | None
  data Variable = Variable { name :: String, typename :: String, value:: Value, isConstant :: Bool }
  data Context = Context { variables :: M.Map String Variable, functions :: M.Map String P.FrogNode, parent :: Maybe Context}
  data Result a = Success a | Error String
  -- call :: String -> Context -> Result (Value, Context)



  getVariable :: Context -> String -> Result Variable
  getVariable _ "" = Error "Empty variable name"
  getVariable ctx name
    | M.notMember name  (variables ctx) = case (parent ctx) of
      Just p -> getVariable p name
      Nothing -> Error ("Could not find variable " ++ name ++ " in context")
    | otherwise = case M.lookup name (variables ctx) of
      Just var -> Success var
      otherwise -> Error ("Could not find variable " ++ name ++ " in context")

  createVariable :: Context -> String -> String -> Value -> Result Context
  createVariable ctx name typename value = Success ctx{variables=newMap}
    where newMap = M.insert name Variable{name=name, typename=typename, value=value, isConstant=False} (variables ctx)

  updateVariable :: Context -> String -> Value -> Result Context
  updateVariable ctx name newValue
    | M.notMember name (variables ctx) = case (parent ctx) of
      Just p -> (case updateVariable p name newValue of Success newP -> Success ctx{parent=Just newP}; Error e -> Error e)
      Nothing -> Error ("Could not update variable " ++ name ++ " because it does not excist")
    | otherwise = Success(ctx{variables=M.update (\var -> Just var{value=newValue}) name (variables ctx)})

  registerFunction :: Context -> P.FrogNode -> Result (Context, Value)
  registerFunction ctx (P.RoutineDefinition{P.name = name, P.params = params, P.body = body})
    | M.notMember name (functions ctx) = Error "Not implemented"
    | otherwise = Error ("Function " ++ name ++ "already defined, cannot define multiple times")

  evaluate :: Context -> P.FrogNode -> Result (Context, Value)
  evaluate ctx (P.Token (L.DigitLiteral digit)) = Success (ctx, NumValue digit)
  evaluate ctx (P.Token (L.StringLiteral string)) = Success (ctx, StringValue string)
  evaluate ctx P.RoutineDefinition {P.name = name, P.params = params, P.body = body} = registerFunction ctx P.RoutineDefinition {P.name = name, P.params = params, P.body = body}

 







  execute :: P.FrogNode -> ()
  execute _ = ()
