module Lexer(lexFrog,FrogToken(..),Keyword(..)) where
import Data.Char (isDigit, isSpace)

data Keyword = Const | Let | Fn | Rt | If | Else | When | Struct | Enum | Infix | Assoc | Return | While | For | Break | Continue | TrueVal | FalseVal
    deriving (Eq, Show)

data FrogToken = Identifier String | DigitLiteral Float | StringLiteral String | Keyword Keyword | Other Char | Operator String | SLComment String | MLComment String | DocComment String | BlockOpen Char | BlockClose Char | None | Endf | Accessor String
    deriving (Eq, Show)

blockOpen :: [Char]
blockOpen = "{[("

isBlockOpen :: Char -> Bool
isBlockOpen ch = ch `elem` blockOpen

blockClose :: [Char]
blockClose = "}])"

isBlockClose :: Char -> Bool
isBlockClose ch = ch `elem` blockClose

operators :: String
operators = "+-*/^&%!~=><"

keywords :: [ String ]
keywords = [ "const", "let", "fn", "rt", "if", "else", "when", "struct", "enum", "infix", "assoc", "return", "true", "false"]


toKeyword :: String -> Maybe Keyword
toKeyword "const" = Just Const
toKeyword "let" = Just Let
toKeyword "fn" = Just Fn
toKeyword "rt" = Just Rt
toKeyword "if" = Just If
toKeyword "else" = Just Else
toKeyword "when" = Just When
toKeyword "struct" = Just Struct
toKeyword "enum" = Just Enum
toKeyword "infix" = Just Infix
toKeyword "assoc" = Just Assoc
toKeyword "return" = Just Return
toKeyword "while" = Just While
toKeyword "for" = Just For
toKeyword "true" = Just TrueVal
toKeyword "false" = Just FalseVal
toKeyword _ = Nothing




isOperator :: Char -> Bool
isOperator ch = ch `elem` operators

isKeyword :: String -> Bool
isKeyword str = case toKeyword str of Nothing -> False; _ -> True;

isAcceptableIdentifier :: Char -> Bool
isAcceptableIdentifier ';' = False
isAcceptableIdentifier ':' = False
isAcceptableIdentifier ',' = False
isAcceptableIdentifier ch = not(isOperator ch) && not(isBlockOpen ch) &&  not(isBlockClose ch)


lexOperator :: String -> String -> [ FrogToken ]
lexOperator [] [] = []
lexOperator [] buffer = [ Operator buffer ]
lexOperator (x : xs) buffer | isOperator x = lexOperator xs (buffer ++ [x])
lexOperator input buffer =  Operator buffer : lexFrog input

lexDigit :: String -> String -> [ FrogToken ]
lexDigit [] [] = []
lexDigit [] buffer = [ DigitLiteral (read buffer :: Float) ]
lexDigit (x: xs) buffer | isDigit x = lexDigit xs (buffer ++ [x])
lexDigit (x: xs) buffer = DigitLiteral (read buffer :: Float) : lexFrog (x:xs)

lexString :: String -> String -> [ FrogToken ]
lexString [] [] = []
lexString [] buffer  = [StringLiteral buffer]
lexString (x : xs) buffer | x == '"' = StringLiteral buffer : lexFrog xs
lexString (x : xs) buffer = lexString xs (buffer ++ [x])


lexIdentifier :: String -> String -> [FrogToken]
lexIdentifier [] [] = []
lexIdentifier [] buffer = [ Identifier buffer ]
lexIdentifier (x : xs) buffer | isSpace x || not(isAcceptableIdentifier x) = case keyword of 
    Nothing -> Identifier buffer : lexFrog (x:xs)
    Just kw -> Keyword kw : lexFrog (x:xs)
    where keyword = toKeyword buffer;

lexIdentifier (x : xs) buffer = lexIdentifier xs (buffer ++ [x])


lexSLComment:: String -> String -> [FrogToken]
lexSLComment ('\n': xs) buffer = SLComment buffer : lexFrog xs
lexSLComment (x: xs) buffer = lexSLComment xs (buffer ++ [x])

lexMLComment:: String -> String -> [FrogToken]
lexMLComment ('*' : '/': xs) buffer = MLComment buffer : lexFrog xs
lexMLComment (x : xs) buffer = lexMLComment xs (buffer ++ [x])

lexDocComment :: String -> String -> [FrogToken]
lexDocComment ('*' : '/': xs) buffer = MLComment buffer : lexFrog xs
lexDocComment (x : xs) buffer = lexDocComment xs (buffer ++ [x])

lexFrog :: String -> [ FrogToken ]
lexFrog [] = []
lexFrog ('.' : '.' : xs) = Operator ".." : lexFrog xs
lexFrog ('.' : xs) = Accessor "." : lexFrog xs


lexFrog ('/' : '/' : xs) = lexSLComment xs ""
lexFrog ('/' : '*' : '*' : xs) = lexDocComment xs ""
lexFrog ('/' : '*' : xs) = lexMLComment xs ""

lexFrog ('"' : xs) = lexString xs ""
lexFrog (x : xs)
  | isBlockOpen x =  BlockOpen x : lexFrog xs
  | isBlockClose x  = BlockClose x : lexFrog xs
  | isSpace x = lexFrog xs
  | isDigit x = lexDigit (x : xs) ""
  | isOperator x = lexOperator xs [x]
  | isAcceptableIdentifier x = lexIdentifier (x : xs) ""
  | otherwise = Other x : lexFrog xs


lexFrog [] = [ Endf ]
lexFrog list = [ None ]
