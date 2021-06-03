{-# LANGUAGE LambdaCase #-}
module Parser(parse, groupBlocks) where
  import qualified Data.Map as M
  import qualified Ast as A

  import qualified Lexer as L

  import Debug.Trace ( trace, traceShow )
  import ListTools(splitWhen)
  import Result(Result(..), mapSuccess)


  closing :: Char -> Char
  closing '{' = '}'
  closing '(' = ')'
  closing '[' = ']'
  closing _ = '\0'

  groupBlocksImpl :: [(Char, [A.FrogNode], Char)] -> L.FrogToken -> [(Char, [A.FrogNode], Char)]
  groupBlocksImpl stack (L.BlockOpen o) = (o, [], closing o) : stack
  groupBlocksImpl ((o, ch, c) : (po, pch, pc) : xs) bcl@(L.BlockClose newToken)
    | c == newToken = (po, pch ++ [A.Block{A.open=o, A.childeren=ch, A.close=c}], pc) : xs
    | otherwise = [('!', [A.ParseError{A.message ="[groupBlocksImpl] No matching block close " ++ [c], A.node = Just (A.Token bcl)}], '!')]

  groupBlocksImpl ((o, ch, c): xs) t = (o, ch ++ [A.Token t] , closing o) : xs


  groupBlocks :: [L.FrogToken] -> A.FrogNode
  groupBlocks tokens =  trace ("groupBlocks" ++ show res )A.Sequence res
    where [ (_, res, _) ] =  foldl groupBlocksImpl [('\0', [], '\0')] tokens

  parseList :: A.FrogNode -> Result [A.FrogNode]
  parseList (A.Statement s) = parseList s
  parseList (A.Sequence seq) = Success seq
  parseList list = Error "Failed to parse list"

  parseArgs :: [A.FrogNode] -> [A.FrogNode]
  
  parseArgs seq = trace ("[parseArgs] input -> " ++ show seq ++ ", splitted ->" ++ show splitted) map (buildTree . A.Sequence) splitted
    where
      splitted = splitWhen (\case A.Token(L.Other ',') -> True; _ -> False) seq
  -- parseArgs node = []



  groupStatements :: A.FrogNode -> A.FrogNode
  groupStatements (A.Sequence []) = A.Sequence []
  groupStatements A.Block {A.open=open, A.childeren=[], A.close=close} = A.Block{A.open=open, A.childeren=[], A.close=close}

  groupStatements b@A.Block{A.open=open, A.childeren=children, A.close=close} = case newChildren of
      A.Sequence seq -> b{A.open=open, A.childeren=[newChildren], A.close=close}
      _ -> b{A.childeren=[newChildren]}
      where newChildren = groupStatements (A.Sequence children)

  groupStatements A.Nop = A.Nop
  groupStatements (A.Sequence seq) = case newSeq of
      [A.Statement(A.Sequence [n])] -> A.Statement n
      _ -> foldl1 A.append newSeq
      where
        newSeq  =  map (A.Statement . A.Sequence) (splitWhen (== A.Token( L.Other ';')) (map groupStatements seq))



  groupStatements (A.Token t) = A.Token t
  groupStatements node = A.Nop

  parseStruct :: [A.FrogNode] -> [A.FrogNode]
  parseStruct (A.Token(L.Identifier n) : A.Token(L.Other ':') : A.Token(L.Identifier tn) : A.Token(L.Other ',') : xs) = A.FieldDefinition{A.name=n, A.typename=tn} : parseStruct xs
  parseStruct [A.Token(L.Identifier n), A.Token(L.Other ':') , A.Token(L.Identifier tn)] = [A.FieldDefinition{A.name=n, A.typename=tn}]
  parseStruct [] = []
  parseStruct n = [A.ParseError{A.message="Failed to parse struct", A.node=Just (A.Sequence n)}]



  buildTree :: A.FrogNode -> A.FrogNode
  buildTree (A.Sequence[A.Sequence seq]) = buildTree (A.Sequence seq)
  buildTree (A.Sequence(A.Token(L.Identifier n): A.Token(L.Operator "=") : rhsValue)) = A.Assignment{A.target=n, A.assignment=buildTree (A.Sequence rhsValue)}
  buildTree (A.Sequence[A.Token(L.Identifier var), A.Token(L.Operator "++")]) = A.Increment (A.Variable var)
  buildTree (A.Sequence[A.Token(L.Identifier var), A.Token(L.Operator "--")]) = A.Decrement (A.Variable var)

  buildTree (A.Sequence(A.Token(L.Identifier lhsValue): A.Token(L.Operator op) : rhsValue))
    | op `elem` ["+=", "-=", "*=", "/="] = A.SelfAssigningInfixCall{A.lhs=A.Variable lhsValue, A.target=op, A.rhs = buildTree (A.Sequence rhsValue)}
    | otherwise = A.InfixCall{A.lhs=A.Variable lhsValue, A.target=op, A.rhs = buildTree (A.Sequence rhsValue)}



  buildTree (A.Statement (A.Sequence seq)) = A.Statement(buildTree (A.Sequence seq))



  buildTree (A.Sequence (A.Token (L.Keyword L.Infix): xs)) = A.InfixDecorator next
      where next = buildTree (A.Sequence xs)

  buildTree (A.Sequence ((A.Token (L.Keyword L.TrueVal)): xs))  = A.append (A.BooleanLiteral True)  (buildTree (A.Sequence xs))
  buildTree (A.Sequence ((A.Token (L.Keyword L.FalseVal)): xs)) = A.append (A.BooleanLiteral False) (buildTree (A.Sequence xs))

  buildTree (A.Sequence (A.Token(L.Keyword keyword) : A.Token(L.Identifier id) : A.Token(L.Operator "=") : xs))
      | keyword == L.Const = A.ConstDeclaration{A.name=id, A.assignment=child, A.typename=""}
      | keyword == L.Let   = A.VarDeclaration{A.name=id, A.assignment=child, A.typename=""}
      where child = buildTree (A.Sequence xs)

  buildTree (A.Sequence (A.Token(L.Keyword keyword): (A.Token(L.Identifier n)) : A.Block{A.open='(', A.childeren=params, A.close=')'} : A.Block{A.open='{', A.childeren=body, A.close='}'} : xs))
      | keyword == L.Fn = case b of
        A.Sequence seq -> A.append A.FunctionDefinition {A.name=n,A.params=parseArgs params, A.body= seq } (buildTree (A.Sequence xs))
        _ -> A.append A.FunctionDefinition {A.name=n, A.params=parseArgs params, A.body=[b]} (buildTree (A.Sequence xs))
      | keyword == L.Rt = case b of
        A.Sequence seq -> A.append A.RoutineDefinition { A.name=n, A.params=parseArgs params, A.body= seq } (buildTree (A.Sequence xs))
        _ -> A.append A.RoutineDefinition { A.name=n, A.params=parseArgs params, A.body=[b]} (buildTree (A.Sequence xs))
      where b = buildTree(A.Sequence body)

  buildTree (A.Sequence [A.Token(L.Keyword keyword)])
      | keyword == L.Return = A.FunctionReturn Nothing

  buildTree (A.Sequence (A.Token(L.Keyword  keyword) : xs))
      | keyword == L.Return = A.FunctionReturn( Just (buildTree (A.Sequence xs)))


  buildTree A.Block{A.open=o, A.childeren=ch, A.close=c} = case newChildren of
    A.Sequence seq -> A.Block{A.open=o, A.childeren=seq, A.close=c}
    _ -> A.Block{A.open=o, A.childeren=[newChildren], A.close=c}
    where newChildren = buildTree (A.Sequence ch)

  buildTree (A.Sequence(A.Token(L.Keyword L.Return) : A.Token(L.Identifier ident) : A.Block{A.open='(', A.childeren=p, A.close=')'} : xs)) =
    A.append (A.FunctionReturn (Just (A.FunctionCall{A.target=ident, A.params= parseArgs p}))) (buildTree (A.Sequence xs))


  buildTree (A.Sequence(A.Token(L.Identifier ident) : A.Block{A.open='(', A.childeren=p, A.close=')'} : xs)) =
    A.append A.FunctionCall{A.target=ident, A.params= parseArgs p} (buildTree (A.Sequence xs))

  buildTree (A.Sequence []) = A.Nop

  buildTree A.Nop = A.Nop
  buildTree (A.Sequence(A.Statement s : xs)) = A.append (buildTree s) (buildTree (A.Sequence xs))



  buildTree (A.Sequence [A.Token (L.StringLiteral str)]) = A.StringLiteral str
  buildTree (A.Sequence [A.Token (L.DigitLiteral num)]) = A.NumericLiteral num
  buildTree (A.Sequence [A.Token (L.IntegerLiteral num)]) = A.IntegerLiteral num
  buildTree (A.Sequence [A.Nop]) = A.Nop
  -- buildTree (Sequence (Token(Keyword Struct) : Token(Identifier n) : Block{open='{' , childs=structBody, close='}'} : xs)) = append (StructDefinition{name=n, fields=parseStruct structBody}) (buildTree (Sequence xs))
  buildTree (A.Sequence (A.Token(L.Keyword L.If) : A.Block{A.open='(' , A.childeren=checkBody, A.close=')'}: A.Block{A.open='{', A.childeren=body, A.close='}'} : xs)) =  case b of
    A.Sequence seq -> A.append (A.IfStatement{A.condition=buildTree (A.Sequence checkBody), A.body=seq, A.next=Nothing}) next
    _ ->   A.append A.IfStatement{A.condition=buildTree (A.Sequence checkBody), A.body=[b], A.next=Nothing} next
    where 
      b = buildTree (A.Sequence body)
      next = (buildTree . A.Sequence) xs

  buildTree (A.Sequence (A.Token(L.Keyword L.While) : A.Block{A.open='(' , A.childeren=checkBody, A.close=')'}: A.Block{A.open='{', A.childeren=body, A.close='}'} : xs)) = case b of
    A.Sequence seq -> A.append A.WhileStatement{A.condition=buildTree (A.Sequence checkBody), A.body=seq} next
    _ -> A.append A.WhileStatement{A.condition=buildTree (A.Sequence checkBody), A.body=[b]} next
    where 
      b = buildTree (A.Sequence body)
      next = (buildTree . A.Sequence) xs
  -- buildTree (Sequence (Token(Identifier ident) : Block{open='{', childs=c, close='}'})) =


  -- buildTree (Sequence(Token(Identifier p) : Token(Accessor acc) : Token(Identifier c) : xs)) = buildTree Sequence(ChildAccessor {parent=p, accessor=acc, child=c} :)
  buildTree (A.Token(L.Identifier id)) = A.Variable{A.name=id}
  buildTree (A.Token(L.StringLiteral str)) = A.StringLiteral str


  buildTree (A.Sequence [node]) = buildTree node
  buildTree node = A.ParseError {A.message="[buildTree] Unknown pattern", A.node=Just node}
  parse :: [L.FrogToken] -> A.FrogNode
  parse tokenStream = buildTree groupedStatements
      where
        groupedBlocks = groupBlocks tokenStream
        groupedStatements = groupStatements groupedBlocks

