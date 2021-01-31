module Parser(parse, FrogNode(..), groupBlocks) where
  import qualified Data.Map as M

  import qualified Lexer as L

  import Debug.Trace ( trace, traceShow )
  import ListTools(splitWhen)
  import Result(Result(..), mapSuccess)


  closing :: Char -> Char
  closing '{' = '}'
  closing '(' = ')'
  closing '[' = ']'
  closing _ = '\0'

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
      | BooleanLiteral Bool
      | NumericLiteral Float
      | StringLiteral String
      | ChildAccessor { parent :: FrogNode, child :: FrogNode, accessor :: String } -- The . accessor.
      | FunctionCall { target :: String, params :: [FrogNode] } -- The call of a function.
      | Assignment { name :: String, assignment :: FrogNode } -- The assignment of an variable in the name segment of the data.
      | Block {open :: Char, childs :: [FrogNode], close :: Char } -- A scope, structured as a block in code, seems like (...) {...} or [...].
      | InfixCall { lhs :: FrogNode, function :: String, rhs :: FrogNode } -- The infix notation of a function call. Is also used for operator operations.
      | SelfAssigningInfixCall { lhs :: FrogNode, function :: String, rhs :: FrogNode } -- Self assigning operator, like +=, -= or even ++ and --;
      | InfixDecorator FrogNode -- Not implemented
      | WhenStatement { input :: FrogNode } -- Not implemented
      | List [ FrogNode ] -- Not implemented
      | TokenStream [L.FrogToken] -- Not implemented
      | Statement FrogNode -- A single statement in code.
      | FunctionReturn FrogNode -- Ends the execution of a function and returns a given value, variable or constant.
      | IfStatement { condition :: FrogNode, body :: [FrogNode], next :: Maybe FrogNode } -- And if statement. next is else (due to keyword reservations in haskell).
      | WhileStatement { condition :: FrogNode, body :: [FrogNode] } -- A while statement.
      | ParseError { message :: String, node :: Maybe FrogNode } -- Gives an error.
      | Dict (M.Map String FrogNode) -- A dictionary
      | Nop -- A non operation.
      deriving(Show, Eq)


  -- | Appends a node to a list of nodes. Combines two or more nodes.
  append :: FrogNode -> FrogNode -> FrogNode 
  append lhs rhs = case (lhs, rhs) of
    (Block{}, Block{}) -> Sequence [lhs, rhs] -- Two blocks will be combined to a sequence of two blocks.
    (Token _, Token _) -> Sequence [lhs, rhs] -- Two tokens will be conbined to a sequence of two tokens
    (Token _, Block{}) -> Sequence [lhs, rhs] -- A token and a block will be combined to a sequence of a token and a block.
    (Block{}, Sequence s) -> Sequence(lhs : s) -- A block and a sequence will be combined to a block appended in the sequence.
    (Block{}, rhs) -> lhs{childs=(childs lhs) ++ [rhs]} -- The block will be combined to 
    (TokenStream str, Token t) -> TokenStream(str ++ [t]) -- Tokens will be appended to a tokenstream
    (Sequence l, Sequence r) -> Sequence(l ++ r) -- 
    ((Sequence seq), _) -> Sequence(seq ++ [rhs])
    (_, (Sequence seq)) -> Sequence(lhs : seq)
    (_, Nop) -> lhs
    (Nop, _) -> rhs
    otherwise -> Sequence[lhs, rhs]




  -- append (Block lhs) (Block rhs) = Sequence [Block lhs, Block rhs]
  -- append (Token lhs) (Token rhs) = Sequence [Token lhs, Token rhs]
  -- append (Token t) (Block b) = Sequence [Token t, Block b]

  -- append (Block lhs) (Sequence rhs) = Sequence(Block lhs : rhs)
  -- append (Block (open, Sequence children, close)) rhs = Block(open, children++[rhs], close)

  -- append (TokenStream str) (Token t) = TokenStream(str ++ [t])
  -- append (Sequence lhs) (Sequence rhs) = Sequence(lhs ++ rhs)
  -- append (Sequence seq) rhs = Sequence (seq ++ [rhs])
  -- append lhs (Sequence rhs) = Sequence(lhs:rhs)
  -- append lhs Nop = lhs
  -- append Nop rhs = rhs


  -- append lhs rhs = Sequence [lhs, rhs]

  -- | 
  groupBlocks :: [L.FrogToken] -> Maybe Char -> ([L.FrogToken], FrogNode)
  groupBlocks (L.BlockOpen open : L.BlockClose close: xs) char | close == closing open =  (buffer, append (Block{open=open, childs=[], close=close}) next)
      where (buffer, next) = groupBlocks xs char

  groupBlocks (L.BlockOpen ch : xs) closeCh = case node of
      Sequence [Sequence x] ->  (resultBuffer, append (Block{open=ch, childs=x
        ,close=c}) nextNode)
      Sequence x -> (resultBuffer, append (Block{open=ch, childs=x,close=c}) nextNode)-- append Block(BlockOpen ch, s, close) nextNode)
      _ -> do {  (next, node) }
      where (L.BlockClose c: next, node) = groupBlocks xs (Just (closing ch)); (resultBuffer, nextNode) = groupBlocks next closeCh


  groupBlocks (L.BlockClose ch: xs) (Just closeCh) | ch /= closeCh = (L.BlockClose ch: xs, Nop)
  groupBlocks (L.BlockClose ch: xs) _ = (L.BlockClose ch:xs,Sequence [])
  groupBlocks [last] _ = ([], Token last)
  groupBlocks (x: xs) closeCh = (next, append (Token x) res )
      where (next, res) = groupBlocks xs closeCh
  groupBlocks [] Nothing = ([], Nop)
  groupBlocks tokens closeCh = (tokens, Nop)


  parseList :: FrogNode -> Result [FrogNode]
  parseList (Statement s) = parseList s
  parseList (Sequence seq) = Success seq
  parseList list = Error "Failed to parse list"

  parseArgs :: [FrogNode] -> [FrogNode]
  parseArgs seq = map buildTree (map Sequence splitted)
    where
      splitted = splitWhen (\x -> case x of Token(L.Other ',') -> True; otherwise -> False) seq
  -- parseArgs node = []



  groupStatements :: FrogNode -> FrogNode
  groupStatements (Sequence []) = Sequence []
  groupStatements Block {open=open, childs=[], close=close} = Block{open=open, childs=[], close=close}

  groupStatements (Block{open=open, childs=children, close=close}) = case newChildren of
      Sequence seq -> Block{open=open, childs=seq, close=close}
      otherwise -> Block{open=open, childs=[newChildren], close=close}
      where newChildren = groupStatements (Sequence children)

  groupStatements Nop = Nop
  groupStatements (Sequence seq) = case newSeq of
      [Statement(Sequence [n])] -> Statement n
      otherwise -> foldl1 append newSeq
      where
        newSeq  =  map (Statement . Sequence) (splitWhen (== Token( L.Other ';')) (map groupStatements seq))



  groupStatements (Token t) = Token t
  groupStatements node = Nop

  parseStruct :: [FrogNode] -> [FrogNode]
  parseStruct (Token(L.Identifier n) : Token(L.Other ':') : Token(L.Identifier tn) : Token(L.Other ',') : xs) = FieldDefinition{name=n, typename=tn} : parseStruct xs
  parseStruct ( [Token(L.Identifier n), Token(L.Other ':') , Token(L.Identifier tn)]) = [FieldDefinition{name=n, typename=tn}]
  parseStruct [] = []
  parseStruct n = [ParseError{message="Failed to parse struct", node=Just (Sequence n)}]

  -- parseFunctionBody :: FrogNode -> Result FrogNode
  -- parseFunctionBody (Block functionBody) = Success Nop
  -- parseFunctionBody _ = Error "Cannot parse functionbody with given type"


  buildTree :: FrogNode -> FrogNode
  buildTree (Sequence[Sequence seq]) = buildTree (Sequence seq)
  buildTree (Sequence(Token(L.Identifier n): Token(L.Operator("=")) : rhsValue)) = Assignment{name=n, assignment=(buildTree (Sequence rhsValue))}
  buildTree (Sequence(Token(L.Identifier lhsValue): Token(L.Operator(op)) : rhsValue))
    | op `elem` ["++", "--", "+=", "-=", "*=", "/="] = SelfAssigningInfixCall{lhs=Variable lhsValue, function=op, rhs = (buildTree (Sequence rhsValue))}
    | otherwise = InfixCall{lhs=Variable lhsValue, function=op, rhs = (buildTree (Sequence rhsValue))}


  buildTree (Statement (Sequence seq)) = Statement(buildTree (Sequence seq))



  buildTree (Sequence (Token (L.Keyword L.Infix): xs)) = InfixDecorator next
      where next = buildTree (Sequence xs)

  buildTree (Sequence ((Token (L.Keyword L.TrueVal)): xs))  = append ((BooleanLiteral True))  (buildTree (Sequence xs))
  buildTree (Sequence ((Token (L.Keyword L.FalseVal)): xs)) = append ((BooleanLiteral False)) (buildTree (Sequence xs))

  buildTree (Sequence (Token(L.Keyword keyword) : Token(L.Identifier id) : Token(L.Operator "=") : xs))
      | keyword == L.Const = ConstDeclaration{name=id,assignment=child,typename=""}
      | keyword == L.Let   = VarDeclaration{name=id, assignment=child, typename=""}
      where child = buildTree (Sequence xs)

  buildTree (Sequence (Token(L.Keyword keyword): (Token(L.Identifier n)) : Block{open='(',childs=params,close=')'} : Block{open='{',childs=body,close='}'} : xs))
      | keyword == L.Fn = case b of
        Sequence seq -> append FunctionDefinition {name=n,params=parseArgs params, body= seq } (buildTree (Sequence xs))
        otherwise -> append FunctionDefinition {name=n,params=parseArgs params, body=[b]} (buildTree (Sequence xs))
      | keyword == L.Rt = case b of
        Sequence seq -> append RoutineDefinition {name=n,params=parseArgs params, body= seq } (buildTree (Sequence xs))
        otherwise -> append RoutineDefinition {name=n,params=parseArgs params, body=[b]} (buildTree (Sequence xs))
      where b = buildTree(Sequence body)

  buildTree (Sequence [Token(L.Keyword keyword)])
      | keyword == L.Return = FunctionReturn Nop

  buildTree (Sequence (Token(L.Keyword  keyword) : xs))
      | keyword == L.Return = FunctionReturn( buildTree (Sequence xs))


  buildTree (Block{open=o, childs=ch, close=c}) = case newChildren of
    Sequence seq -> Block{open=o, childs=seq, close=c}
    otherwise -> Block{open=o, childs=[newChildren], close=c}
    where newChildren = buildTree (Sequence ch)

  buildTree (Sequence(Token(L.Identifier ident) : Block{open='(', childs=p, close=')'} : xs)) = append FunctionCall{target=ident, params= parseArgs p} (buildTree (Sequence xs))
  buildTree (Sequence []) = Nop

  buildTree Nop = Nop
  buildTree (Sequence(Statement s : xs)) = append (buildTree s) (buildTree (Sequence xs))



  buildTree (Sequence [Token (L.StringLiteral str)]) = StringLiteral str
  buildTree (Sequence [Token (L.DigitLiteral num)]) = NumericLiteral num
  buildTree (Sequence [Nop]) = Nop
  -- buildTree (Sequence (Token(Keyword Struct) : Token(Identifier n) : Block{open='{' , childs=structBody, close='}'} : xs)) = append (StructDefinition{name=n, fields=parseStruct structBody}) (buildTree (Sequence xs))
  buildTree (Sequence (Token(L.Keyword L.If) : Block{open='(' , childs=checkBody, close=')'}: Block{open='{', childs=body, close='}'} : xs)) =  case b of
    Sequence seq -> IfStatement{condition=buildTree (Sequence checkBody), body=seq, next=Nothing}
    otherwise -> IfStatement{condition=buildTree (Sequence checkBody), body=[b], next=Nothing}
    where b = buildTree (Sequence body)

  buildTree (Sequence (Token(L.Keyword L.While) : Block{open='(' , childs=checkBody, close=')'}: Block{open='{', childs=body, close='}'} : xs)) = case b of
    Sequence seq -> WhileStatement{condition=buildTree (Sequence checkBody), body=seq}
    otherwise -> WhileStatement{condition=buildTree (Sequence checkBody), body=[b]}
    where b = buildTree (Sequence body)
  -- buildTree (Sequence (Token(Identifier ident) : Block{open='{', childs=c, close='}'})) =


  -- buildTree (Sequence(Token(Identifier p) : Token(Accessor acc) : Token(Identifier c) : xs)) = buildTree Sequence(ChildAccessor {parent=p, accessor=acc, child=c} :)
  buildTree (Token(L.Identifier id)) = Variable{name=id}
  buildTree (Token(L.StringLiteral str)) = StringLiteral str


  buildTree (Sequence [node]) = buildTree node
  buildTree node = ParseError {message="Unknown pattern", node=Just node}
  parse :: [L.FrogToken] ->([L.FrogToken], FrogNode)
  parse tokenStream = (buffer, buildTree (groupStatements grouped))
      where (buffer , grouped) = groupBlocks tokenStream Nothing;