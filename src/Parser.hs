module Parser(parse, FrogNode, groupBlocks) where


import Lexer(lexFrog,FrogToken(..),Keyword(..))

import Debug.Trace ( trace, traceShow )
import ListTools(splitWhen)

closing :: Char -> Char
closing '{' = '}'
closing '(' = ')'
closing '[' = ']'
closing _ = '\0'

data FrogNode = Sequence [FrogNode]
    | Token FrogToken
    | ConstDeclaration { name :: FrogToken, assignment :: FrogNode, typename :: FrogNode }
    | VarDeclaration { name :: FrogToken, assignment :: FrogNode, typename :: FrogNode }
    | ParameterDefinition { name :: FrogToken, typename :: FrogNode }
    | ParameterList [FrogNode]
    | ArrayDefinition [FrogNode]
    | RoutineDefinition { name :: FrogToken, params :: FrogNode, body :: FrogNode}
    | FunctionDefinition { name :: FrogToken, params :: FrogNode, body :: FrogNode }
    | FunctionCall {target :: FrogNode, params :: FrogNode}
    | Assignment (FrogNode, FrogNode)
    | Block (FrogToken, FrogNode, FrogToken)
    | InfixCall { lhs :: FrogNode, function :: FrogToken, rhs :: FrogNode }
    | InfixDecorator FrogNode
    | WhenStatement { input :: FrogNode }
    | List [ FrogNode ]
    | TokenStream [FrogToken]
    | Statement FrogNode
    | FunctionReturn FrogNode
    | ParseError { message :: String, node :: Maybe FrogNode }
    | Nop
    deriving(Show, Eq)


append :: FrogNode -> FrogNode -> FrogNode

append (Block lhs) (Block rhs) = Sequence [Block lhs, Block rhs]
append (Token lhs) (Token rhs) = Sequence [Token lhs, Token rhs]
append (Token t) (Block b) = Sequence [Token t, Block b]

append (Block lhs) (Sequence rhs) = Sequence(Block lhs : rhs)
append (Block (open, Sequence children, close)) rhs = Block(open, Sequence (children++[rhs]), close)

append (TokenStream str) (Token t) = TokenStream(str ++ [t])
append (Sequence lhs) (Sequence rhs) = Sequence(lhs ++ rhs)
append (Sequence seq) rhs = Sequence (seq ++ [rhs])
append lhs (Sequence rhs) = Sequence(lhs:rhs)
append lhs Nop = lhs
append Nop rhs = rhs


append lhs rhs = Sequence [lhs, rhs]

groupBlocks :: [FrogToken] -> Maybe Char -> ([FrogToken], FrogNode)


groupBlocks (BlockOpen open : BlockClose close: xs) char | close == closing open =  (buffer, append (Block(BlockOpen open, Nop, BlockClose close)) next)
    where (buffer, next) = groupBlocks xs char

groupBlocks (BlockOpen ch : xs) closeCh = case node of 
    Sequence _ -> (resultBuffer, append (Block(BlockOpen ch, node ,close)) nextNode)-- append Block(BlockOpen ch, s, close) nextNode)
    _ -> do { traceShow node  (next, node) }
    where (close: next, node) = groupBlocks xs (Just (closing ch)); (resultBuffer, nextNode) = groupBlocks next closeCh


groupBlocks (BlockClose ch: xs) (Just closeCh) | ch /= closeCh = (BlockClose ch: xs, Nop)
groupBlocks (BlockClose ch: xs) _ = (BlockClose ch:xs,Sequence [])
groupBlocks [last] _ = ([], Token last)
groupBlocks (x: xs) closeCh = (next, append (Token x) res )
    where (next, res) = groupBlocks xs closeCh
groupBlocks [] Nothing = ([], Nop)
groupBlocks tokens closeCh = trace "groupBlocks Failed" traceShow tokens traceShow closeCh (tokens, Nop)


parseList :: FrogNode -> FrogNode
parseList (Statement s) = parseList s
parseList (Sequence seq) = Sequence seq
parseList list = ParseError {message="Failed to parse list",node=Just list}



groupStatements :: FrogNode -> FrogNode
groupStatements (Sequence []) = Sequence []
groupStatements (Block(open, children, close)) = case children of 
    Sequence [] -> Block(open, Nop, close)
    _ -> Block(open, groupStatements children, close)

groupStatements Nop = Nop
groupStatements (Sequence seq) =  foldl1 append newSeq
    where newSeq  =  map (Statement . Sequence) (splitWhen (== Token( Other ';')) (map groupStatements seq))
groupStatements (Token t) = Token t
groupStatements node = trace "!groupStatements failed!" traceShow node Nop

parseStruct :: FrogNode -> FrogNode
parseStruct structNode = structNode


buildTree :: FrogNode -> FrogNode


buildTree (Statement (Sequence seq)) = Statement(buildTree (Sequence seq))



buildTree (Sequence (Token (Keyword Infix): xs)) = InfixDecorator next
    where next = buildTree (Sequence xs)


buildTree (Sequence (Token(Keyword keyword) : Token(Identifier id) : Token(Operator "=") : xs))
    | keyword == Const = ConstDeclaration{name=Identifier id,assignment=child,typename=Nop}
    | keyword == Let   = VarDeclaration{name=Identifier id, assignment=child, typename=Nop}
    where child = buildTree (Sequence xs)

buildTree (Sequence (Token(Keyword keyword): (Token(Identifier name)) : Block params : Block body : xs)) 
    | keyword == Fn = append FunctionDefinition {name=Identifier name,params=buildTree (Block params), body= buildTree(Block body) } (buildTree (Sequence xs))
    | keyword == Rt = append RoutineDefinition {name=Identifier name,params=buildTree (Block params), body= buildTree(Block body) } (buildTree (Sequence xs))

buildTree (Sequence [Token(Keyword keyword)]) 
    | keyword == Return = FunctionReturn Nop

buildTree (Sequence (Token(Keyword  keyword) : xs))
    | keyword == Return = FunctionReturn( buildTree (Sequence xs))


buildTree (Block(open, children, close)) = Block(open, newChildren, close)
    where newChildren = buildTree children 

buildTree (Sequence(Token(Identifier ident) : Block(BlockOpen '(', children, BlockClose ')') : xs)) = append FunctionCall{ target=Token(Identifier ident), params= parseList children} (buildTree (Sequence xs))
buildTree (Sequence []) = Nop
buildTree Nop = Nop
buildTree (Sequence(Statement s : xs)) = append (buildTree s) (buildTree (Sequence xs))
buildTree (Sequence [Token (StringLiteral str)]) = Token (StringLiteral str)
buildTree (Sequence [Token (DigitLiteral str)]) = Token (DigitLiteral str)
buildTree (Sequence [Nop]) = Nop
buildTree (Token(Identifier id)) = Token(Identifier id)
buildTree node = ParseError {message="Unknown pattern", node=Just node}

parse :: [FrogToken] ->([FrogToken], FrogNode)
parse tokenStream = (buffer, buildTree (groupStatements grouped))
    where (buffer , grouped) = groupBlocks tokenStream Nothing; 

