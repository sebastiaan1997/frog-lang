module Parser(parse, FrogNode, groupBlocks) where

import qualified Lexer

import Debug.Trace ( trace, traceShow )
import ListTools(splitWhen)

closing :: Char -> Char
closing '{' = '}'
closing '(' = ')'
closing '[' = ']'
closing _ = '\0'

data FrogNode = Sequence [FrogNode]
    | Token Lexer.FrogToken
    | ConstDeclaration { name :: Lexer.FrogToken, assignment :: FrogNode, typename :: FrogNode }
    | VarDeclaration { name :: Lexer.FrogToken, assignment :: FrogNode, typename :: FrogNode }
    | ParameterDefinition { name :: Lexer.FrogToken, typename :: FrogNode }
    | ParameterList [FrogNode]
    | ArrayDefinition [FrogNode]
    | FunctionDefinition { name :: Lexer.FrogToken, params :: FrogNode, body :: FrogNode }
    | FunctionCall (FrogNode, FrogNode)
    | Assignment (FrogNode, FrogNode)
    | Block (Lexer.FrogToken, [FrogNode], Lexer.FrogToken)
    | InfixCall { lhs :: FrogNode, function :: Lexer.FrogToken, rhs :: FrogNode }
    | None
    | TokenStream [Lexer.FrogToken]
    | Statement [FrogNode]
    deriving(Show, Eq)

append :: FrogNode -> FrogNode -> FrogNode
append (Token lhs) (Token rhs) = Sequence [Token lhs, Token rhs]
append (Token t) (Block b) = Sequence [Token t, Block b]
append (Block (open, children, close)) rhs = Block(open, children++[rhs], close)
append (TokenStream str) (Token t) = TokenStream(str ++ [t])
append (Sequence seq) rhs = Sequence (seq ++ [rhs])
append lhs (Sequence rhs) = Sequence(lhs:rhs)
append lhs None = lhs

append lhs rhs = do {
    trace "append failed"
    traceShow lhs
    traceShow rhs
    None
}








groupBlocks :: [Lexer.FrogToken] -> Char -> ([Lexer.FrogToken], FrogNode)
groupBlocks [last] _ = ([], Token last)
groupBlocks (Lexer.BlockOpen ch : xs) _ = case node of 
    Sequence s -> (next,  Block (Lexer.BlockOpen ch, s, close))
    otherwise -> do { traceShow node  (next, node) }
    where (close: next, node) = groupBlocks xs (closing ch)
groupBlocks (Lexer.BlockClose ch: xs) closeCh | ch /= closeCh = (Lexer.BlockClose ch: xs, None)
groupBlocks (Lexer.BlockClose ch: xs) closeCh = (Lexer.BlockClose ch:xs,Sequence [])
groupBlocks (x: xs) closeCh = (next, append (Token x) res )
    where (next, res) = groupBlocks xs closeCh

groupStatements :: FrogNode -> FrogNode
groupStatements (Sequence seq)  = Sequence(map (\x -> Statement x) splitted)
    where splitted = splitWhen (\x -> x == Token(Lexer.Other ';')) seq

groupStatements (Block(open, children, close)) = case newChildren of 
    Sequence s -> Block(open, s, close)
    _ -> Block(open, children, close)
    where newChildren = groupStatements (Sequence children)
        


buildTree :: FrogNode -> FrogNode
buildTree (Block(open,children,close)) = case new of
    Sequence s -> Block(open, s ,close)
    _ -> Block(open,children,close)
    where new = buildTree (Sequence children)

buildTree (Statement s) = case s of
    (Token(Lexer.Identifier ident) : Block(Token(Lexer.BlockOpen '('), parameters : Token(Lexer.BlockClose ')')), xs) -> None



buildTree node = node



-- parse :: FrogNode -> FrogNode
-- parse (Sequence s) = Sequence(map parse s)
-- parse (Token(Lexer.Keyword(Lexer.Const)) : Lexer.Identifier identifier : Lexer.Operator '=' : xs) = Parse Sequence [(ConstDeclaration{name=identifier,assignment=None,typename=None}) , parse xs]
-- parse (Token(Lexer.Identifier(lhs)) : op : Token(Lexer.Identifier(rhs): xs) = 
parse :: [Lexer.FrogToken] -> FrogNode
parse tokenStream = buildTree (groupStatements grouped)
    where (_, grouped) = groupBlocks tokenStream '\0'; 






-- Debugging





