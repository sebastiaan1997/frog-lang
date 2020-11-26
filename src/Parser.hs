module Parser where
import qualified Lexer



data FrogNode = 
    TokenSequence [FrogNode]
    | Token (Lexer.FrogToken) 
    | Block { open :: Lexer.FrogToken, childs :: [FrogNode], close :: Lexer.FrogToken }
    | None

    deriving(Show)




composeBlocks :: FrogNode -> Lexer.FrogToken -> FrogNode
composeBlocks TokenSequence((x : xs)) token =  (composeBlocks TokenSequence(xs) Lexer.None) ++ [x]






    