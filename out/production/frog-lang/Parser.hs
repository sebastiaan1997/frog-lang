module Parser(parse, FrogNode(..), groupBlocks) where
  import qualified Data.Map as M

  import Lexer(lexFrog,FrogToken(..),Keyword(..))

  import Debug.Trace ( trace, traceShow )
  import ListTools(splitWhen)
  import Result(Result(..), mapSuccess)


  closing :: Char -> Char
  closing '{' = '}'
  closing '(' = ')'
  closing '[' = ']'
  closing _ = '\0'

  data FrogNode = Sequence [FrogNode]
      | Token FrogToken
      | ConstDeclaration { name :: String, assignment :: FrogNode, typename :: String }
      | VarDeclaration { name :: String, assignment :: FrogNode, typename :: String }
      | FieldDefinition { name :: String, typename :: String }
      | ParameterDefinition { name :: String, typename :: String }
      | ParameterList [FrogNode]
      | ArrayDefinition [FrogNode]
      | RoutineDefinition { name :: String, params :: [FrogNode], body :: [FrogNode] }
      | FunctionDefinition { name :: String, params :: [FrogNode], body :: [FrogNode] }
      | StructDefinition { name :: String, fields :: [FrogNode] }
      | StructInstance { name :: String, fields :: [FrogNode] }
      | Variable { name :: String }
      | ChildAccessor { parent :: FrogNode, child :: FrogNode, accessor :: String }
      | FunctionCall { target :: String, params :: [FrogNode] }
      | Assignment { name :: String, assignment :: FrogNode }
      | Block {open :: Char, childs :: [FrogNode], close :: Char }
      | InfixCall { lhs :: FrogNode, function :: String, rhs :: FrogNode }
      | SelfAssigningInfixCall { lhs :: FrogNode, function :: String, rhs :: FrogNode }
      | InfixDecorator FrogNode
      | WhenStatement { input :: FrogNode }
      | List [ FrogNode ]
      | TokenStream [FrogToken]
      | Statement FrogNode
      | FunctionReturn FrogNode
      | IfStatement { condition :: FrogNode, body :: [FrogNode], next :: Maybe FrogNode }
      | WhileStatement { condition :: FrogNode, body :: [FrogNode] }
      | ParseError { message :: String, node :: Maybe FrogNode }
      | Dict (M.Map String FrogNode)
      | Nop
      deriving(Show, Eq)



  append :: FrogNode -> FrogNode -> FrogNode
  append lhs rhs = case (lhs, rhs) of
    (Block{}, Block{}) -> Sequence [lhs, rhs]
    (Token _, Token _) -> Sequence [lhs, rhs]
    (Token _, Block{}) -> Sequence [lhs, rhs]
    (Block{}, Sequence s) -> Sequence(lhs : s)
    (Block{}, rhs) -> lhs{childs=(childs lhs) ++ [rhs]}
    (TokenStream str, Token t) -> TokenStream(str ++ [t])
    (Sequence l, Sequence r) -> Sequence(l ++ r)
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

  groupBlocks :: [FrogToken] -> Maybe Char -> ([FrogToken], FrogNode)
  groupBlocks (BlockOpen open : BlockClose close: xs) char | close == closing open =  (buffer, append (Block{open=open, childs=[], close=close}) next)
      where (buffer, next) = groupBlocks xs char

  groupBlocks (BlockOpen ch : xs) closeCh = case node of
      Sequence [Sequence x] ->  (resultBuffer, append (Block{open=ch, childs=x
        ,close=c}) nextNode)
      Sequence x -> (resultBuffer, append (Block{open=ch, childs=x,close=c}) nextNode)-- append Block(BlockOpen ch, s, close) nextNode)
      _ -> do {  (next, node) }
      where (BlockClose c: next, node) = groupBlocks xs (Just (closing ch)); (resultBuffer, nextNode) = groupBlocks next closeCh


  groupBlocks (BlockClose ch: xs) (Just closeCh) | ch /= closeCh = (BlockClose ch: xs, Nop)
  groupBlocks (BlockClose ch: xs) _ = (BlockClose ch:xs,Sequence [])
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
      splitted = splitWhen (\x -> case x of Token(Other ',') -> True; otherwise -> False) seq
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
        newSeq  =  map (Statement . Sequence) (splitWhen (== Token( Other ';')) (map groupStatements seq))



  groupStatements (Token t) = Token t
  groupStatements node = Nop

  parseStruct :: [FrogNode] -> [FrogNode]
  parseStruct (Token(Identifier n) : Token(Other ':') : Token(Identifier tn) : Token(Other ',') : xs) = FieldDefinition{name=n, typename=tn} : parseStruct xs
  parseStruct ( [Token(Identifier n), Token(Other ':') , Token(Identifier tn)]) = [FieldDefinition{name=n, typename=tn}]
  parseStruct [] = []
  parseStruct n = [ParseError{message="Failed to parse struct", node=Just (Sequence n)}]

  -- parseFunctionBody :: FrogNode -> Result FrogNode
  -- parseFunctionBody (Block functionBody) = Success Nop
  -- parseFunctionBody _ = Error "Cannot parse functionbody with given type"


  buildTree :: FrogNode -> FrogNode
  buildTree (Sequence[Sequence seq]) = buildTree (Sequence seq)
  buildTree (Sequence(Token(Identifier n): Token(Operator("=")) : rhsValue)) = Assignment{name=n, assignment=(buildTree (Sequence rhsValue))}
  buildTree (Sequence(Token(lhsValue): Token(Operator(op)) : rhsValue))
    | op `elem` ["++", "--", "+=", "-=", "*=", "/="] = SelfAssigningInfixCall{lhs=(buildTree (Token lhsValue)), function=op, rhs = (buildTree (Sequence rhsValue))}
    | otherwise = InfixCall{lhs=Token(lhsValue), function=op, rhs = (buildTree (Sequence rhsValue))}


  buildTree (Statement (Sequence seq)) = Statement(buildTree (Sequence seq))



  buildTree (Sequence (Token (Keyword Infix): xs)) = InfixDecorator next
      where next = buildTree (Sequence xs)

  buildTree (Sequence ((Token (Keyword TrueVal)): xs)) = append ((Token (Keyword TrueVal))) (buildTree (Sequence xs))
  buildTree (Sequence ((Token (Keyword FalseVal)): xs)) = append ((Token (Keyword FalseVal))) (buildTree (Sequence xs))

  buildTree (Sequence (Token(Keyword keyword) : Token(Identifier id) : Token(Operator "=") : xs))
      | keyword == Const = ConstDeclaration{name=id,assignment=child,typename=""}
      | keyword == Let   = VarDeclaration{name=id, assignment=child, typename=""}
      where child = buildTree (Sequence xs)

  buildTree (Sequence (Token(Keyword keyword): (Token(Identifier n)) : Block{open='(',childs=params,close=')'} : Block{open='{',childs=body,close='}'} : xs))
      | keyword == Fn = case b of
        Sequence seq -> append FunctionDefinition {name=n,params=parseArgs params, body= seq } (buildTree (Sequence xs))
        otherwise -> append FunctionDefinition {name=n,params=parseArgs params, body=[b]} (buildTree (Sequence xs))
      | keyword == Rt = case b of
        Sequence seq -> append RoutineDefinition {name=n,params=parseArgs params, body= seq } (buildTree (Sequence xs))
        otherwise -> append RoutineDefinition {name=n,params=parseArgs params, body=[b]} (buildTree (Sequence xs))
      where b = buildTree(Sequence body)

  buildTree (Sequence [Token(Keyword keyword)])
      | keyword == Return = FunctionReturn Nop

  buildTree (Sequence (Token(Keyword  keyword) : xs))
      | keyword == Return = FunctionReturn( buildTree (Sequence xs))


  buildTree (Block{open=o, childs=ch, close=c}) = case newChildren of
    Sequence seq -> Block{open=o, childs=seq, close=c}
    otherwise -> Block{open=o, childs=[newChildren], close=c}
    where newChildren = buildTree (Sequence ch)

  buildTree (Sequence(Token(Identifier ident) : Block{open='(', childs=p, close=')'} : xs)) = append FunctionCall{target=ident, params= parseArgs p} (buildTree (Sequence xs))
  buildTree (Sequence []) = Nop

  buildTree Nop = Nop
  buildTree (Sequence(Statement s : xs)) = append (buildTree s) (buildTree (Sequence xs))



  buildTree (Sequence [Token (StringLiteral str)]) = Token (StringLiteral str)
  buildTree (Sequence [Token (DigitLiteral str)]) = Token (DigitLiteral str)
  buildTree (Sequence [Nop]) = Nop
  buildTree (Sequence (Token(Keyword Struct) : Token(Identifier n) : Block{open='{' , childs=structBody, close='}'} : xs)) = append (StructDefinition{name=n, fields=parseStruct structBody}) (buildTree (Sequence xs))
  buildTree (Sequence (Token(Keyword If) : Block{open='(' , childs=checkBody, close=')'}: Block{open='{', childs=body, close='}'} : xs)) =  case b of
    Sequence seq -> IfStatement{condition=buildTree (Sequence checkBody), body=seq, next=Nothing}
    otherwise -> IfStatement{condition=buildTree (Sequence checkBody), body=[b], next=Nothing}
    where b = buildTree (Sequence body)

  buildTree (Sequence (Token(Keyword While) : Block{open='(' , childs=checkBody, close=')'}: Block{open='{', childs=body, close='}'} : xs)) = case b of
    Sequence seq -> WhileStatement{condition=buildTree (Sequence checkBody), body=seq}
    otherwise -> WhileStatement{condition=buildTree (Sequence checkBody), body=[b]}
    where b = buildTree (Sequence body)
  -- buildTree (Sequence (Token(Identifier ident) : Block{open='{', childs=c, close='}'})) =


  -- buildTree (Sequence(Token(Identifier p) : Token(Accessor acc) : Token(Identifier c) : xs)) = buildTree Sequence(ChildAccessor {parent=p, accessor=acc, child=c} :)
  buildTree (Token(Identifier id)) = Variable{name=id}
  buildTree (Token(StringLiteral str)) = Token (StringLiteral str)


  buildTree (Sequence [node]) = buildTree node
  buildTree node = ParseError {message="Unknown pattern", node=Just node}
  parse :: [FrogToken] ->([FrogToken], FrogNode)
  parse tokenStream = (buffer, buildTree (groupStatements grouped))
      where (buffer , grouped) = groupBlocks tokenStream Nothing;