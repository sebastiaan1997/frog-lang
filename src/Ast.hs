module Ast(FrogNode(..), applyConditional, applyChilderen, replaceNodes, append, isValue, isLiteral) where
    import qualified Lexer as L
    import qualified Data.Map as M
    import Data.Maybe ( mapMaybe )
    


    data FrogNode = Sequence [FrogNode]
        | Token L.FrogToken -- A single token as parsed by the lexer.
        | ConstDeclaration { name :: String, assignment :: FrogNode, typename :: String } -- The declaration of a constant.
        | VarDeclaration { name :: String, assignment :: FrogNode, typename :: String } -- The declaration of a variable.
        | FieldDefinition { name :: String, typename :: String } -- The definition of a field inside a struct.
        | ParameterDefinition { name :: String, typename :: String } -- The definition of a parameter of a method, function or routine.
        | ParameterList [FrogNode] -- The list of parameters associated with a function.
        | ArrayDefinition [FrogNode] -- The definition of an array or list.
        | RoutineDefinition { name :: String, params :: [FrogNode], body :: [FrogNode] } -- The definition of a routine in code.
        | FunctionDefinition { name :: String, params :: [FrogNode], body :: [FrogNode] } -- The definition of a function in code
        | StructInstance { name :: String, fields :: [FrogNode] } -- The definition of the instanciation of a struct NOT IMPLEMENTED.
        | Variable { name :: String } -- Variable or constant.
        | BooleanLiteral Bool -- Boolean literal
        | NumericLiteral Float -- Numeric literal, used for floats.
        | IntegerLiteral Int -- Integer literal
        | StringLiteral String -- The definition of a string literal
        | ChildAccessor { parent :: FrogNode, child :: FrogNode, accessor :: String } -- The . accessor.
        | FunctionCall { target :: String, params :: [FrogNode] } -- The call of a function.
        | Assignment { target :: String, assignment :: FrogNode } -- The assignment of an variable in the name segment of the data.
        | Block {open :: Char, childeren :: [FrogNode], close :: Char } -- A scope, structured as a block in code, seems like (...) {...} or [...].
        | InfixCall { lhs :: FrogNode, target :: String, rhs :: FrogNode } -- The infix notation of a function call. Is also used for operator operations.
        | SelfAssigningInfixCall { lhs :: FrogNode, target :: String, rhs :: FrogNode } -- Self assigning operator, like +=, -= or even ++ and --;
        | Increment { lhs :: FrogNode } -- Increment a given node
        | Decrement { lhs :: FrogNode } -- Decrement a given node
        | InfixDecorator FrogNode -- Gives a infix decorator for a function, Not implemented
        | WhenStatement { input :: FrogNode } -- A Rust like pattern matching statement, Not implemented
        | List [ FrogNode ] -- A list descriptor, Not implemented
        | TokenStream [L.FrogToken] -- Token stream Not implemented
        | Statement FrogNode -- A single statement in code.
        | FunctionReturn (Maybe FrogNode) -- Ends the execution of a function and returns a given value, variable or constant.
        | IfStatement { condition :: FrogNode, body :: [FrogNode], next :: Maybe FrogNode } -- And if statement. next is else (due to keyword reservations in haskell).
        | WhileStatement { condition :: FrogNode, body :: [FrogNode] } -- A while statement.
        | ParseError { message :: String, node :: Maybe FrogNode } -- Gives an error.
        | Dict (M.Map String FrogNode) -- A dictionary, not implemented.
        | Nop -- A non operation. Does absolutely nothing.
        deriving(Show, Eq)


    applyConditional :: (FrogNode -> Bool) -> (FrogNode -> Maybe FrogNode) -> FrogNode -> Maybe FrogNode
    applyConditional condition functor target = if condition target then functor target else Just target -- Applied a function depending at the condition function


    -- Apply function to childerenconvertData
    applyChilderen :: (FrogNode -> Maybe FrogNode) -> FrogNode -> Maybe FrogNode
    applyChilderen functor (Sequence sequence) = Just (Sequence (mapMaybe functor sequence))
    applyChilderen functor ConstDeclaration{ name=n, assignment=a, typename=t} = case functor a of
        Just newA -> Just ConstDeclaration{name=n, assignment=newA, typename=t}
        _ -> Nothing
    
    applyChilderen functor VarDeclaration{ name=n, assignment=a, typename=t } = case functor a of
        Just newA -> Just VarDeclaration{ name=n, assignment=newA, typename=t }
        _ -> Nothing
    applyChilderen functor (ParameterList params) = Just (ParameterList (mapMaybe functor params))
    applyChilderen functor (ArrayDefinition params) = Just (ArrayDefinition (mapMaybe functor params))
    applyChilderen functor RoutineDefinition{name=n, params=p, body=b} = Just RoutineDefinition{name=n, params=mapMaybe functor p, body=mapMaybe functor b}    
    applyChilderen functor FunctionDefinition{name=n, params=p, body=b} = Just FunctionDefinition{name=n, params=mapMaybe functor p, body=mapMaybe functor b}
    applyChilderen functor StructInstance{name=n, fields=f} = Just StructInstance{name=n, fields= mapMaybe functor f}
    applyChilderen functor FunctionCall{target=t, params=p} = Just FunctionCall{target=t, params=mapMaybe functor p }
    applyChilderen functor Assignment{target=n, assignment=a} = case functor a of
        Just newA -> Just Assignment{target=n, assignment=newA}
        _ -> Nothing

    applyChilderen functor Block{open=o, childeren=ch, close=c} = Just Block{open=o, childeren=mapMaybe functor ch, close=c}
    applyChilderen functor InfixCall{lhs=l, target=t, rhs=r} = case (functor l, functor r) of
        (Just newL, Just newR) -> Just InfixCall{lhs=newL, target=t, rhs=newR}
        _ -> Nothing

    applyChilderen functor SelfAssigningInfixCall{lhs=l , target=t, rhs=r} = case (functor l, functor r) of
        (Just newL, Just newR) -> Just SelfAssigningInfixCall{lhs=newL, target=t, rhs=newR}
        _ -> Nothing
    
    applyChilderen functor (InfixDecorator inf) = functor inf
    applyChilderen functor (Statement stmt) = functor stmt
    applyChilderen functor (FunctionReturn functionReturn) = case functionReturn of 
        Just fr ->  Just (FunctionReturn (functor fr))
        _ -> Nothing
    applyChilderen functor IfStatement{ condition=c, body=b, next=Just next } = case functor c of
        Just newCondition -> Just IfStatement{ condition=newCondition, body=mapMaybe functor b, next= functor next }
        _ -> Nothing

    applyChilderen functor IfStatement{ condition=c, body=b, next=Nothing } = case functor c of
        Just newCondition -> Just IfStatement{ condition=newCondition, body=mapMaybe functor b, next=Nothing }
        _ -> Nothing 


    applyChilderen functor WhileStatement{ condition=c, body=b } = case functor c of
        Just newCondition -> Just WhileStatement{ condition=newCondition, body=mapMaybe functor b}
        _ -> Nothing     
    
    applyChilderen functor node = Just node

    -- Replace nodes in token tree.
    replaceNodes :: FrogNode -> FrogNode -> FrogNode -> Maybe FrogNode
    replaceNodes target replacement tree = if tree == target then Just replacement else applyChilderen (replaceNodes target replacement) tree


    append :: FrogNode -> FrogNode -> FrogNode
    append lhs rhs = case (lhs, rhs) of
        (Block{}, Block{}) -> Sequence [lhs, rhs] -- Two blocks will be combined to a sequence of two blocks.
        (Token _, Token _) -> Sequence [lhs, rhs] -- Two tokens will be conbined to a sequence of two tokens
        (Token _, Block{}) -> Sequence [lhs, rhs] -- A token and a block will be combined to a sequence of a token and a block.
        (Block{}, Sequence s) -> Sequence(lhs : s) -- A block and a sequence will be combined to a block appended in the sequence.
        (Block{}, rhs) -> lhs{childeren=childeren lhs ++ [rhs]} -- The block will be combined to 
        (TokenStream str, Token t) -> TokenStream(str ++ [t]) -- Tokens will be appended to a tokenstream
        (Sequence l, Sequence r) -> Sequence(l ++ r) -- 
        (Sequence seq, _) -> Sequence(seq ++ [rhs])
        (_, Sequence seq) -> Sequence(lhs : seq)
        (_, Nop) -> lhs
        (Nop, _) -> rhs
        _ -> Sequence[lhs, rhs]

    -- | Checks wether a node is a value node
    isValue :: FrogNode -> Bool
    -- | A boolean literal is a value node
    isValue (BooleanLiteral _) = True
    -- | A numeric literal is a value node
    isValue (NumericLiteral _) = True
    -- | A string literal is a value node
    isValue (StringLiteral _)  = True
    -- | A integer literal is a value node
    isValue (IntegerLiteral _) = True
    -- | A variable is a value node.
    isValue (Variable _) = True
    -- | Everything else is not a value node
    isValue _ = False

    isLiteral :: FrogNode -> Bool
    isLiteral (BooleanLiteral _) = True
    isLiteral (NumericLiteral _) = True
    isLiteral (StringLiteral _)  = True
    isLiteral (IntegerLiteral _) = True
    isLiteral _ = False