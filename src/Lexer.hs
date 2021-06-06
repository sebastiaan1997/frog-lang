module Lexer(lexFrog,FrogToken(..),Keyword(..)) where
import Data.Char (isDigit, isSpace)

-- | Definition of the language keywords
data Keyword = Const | Let | Fn | Rt | If | Else | When | Struct | Enum | Infix | Assoc | Return | While | For | Break | Continue | TrueVal | FalseVal
    deriving (Eq, Show)
-- | Definition of the type of the tokens
data FrogToken = Identifier String | IntegerLiteral Int | DigitLiteral Float | StringLiteral String | Keyword Keyword | Other Char | Operator String | SLComment String | MLComment String | DocComment String | BlockOpen Char | BlockClose Char | None | Endf | Accessor String
    deriving (Eq, Show)

-- | Definition of the characters that may open a code block
blockOpen :: [Char]
blockOpen = "{[("
-- | Function that judges if a given character might be a block opening token.
isBlockOpen :: Char -> Bool
isBlockOpen ch = ch `elem` blockOpen

-- | Definition of the characters that may close a code block
blockClose :: [Char]
blockClose = "}])"
-- | Function that judges if a given character might be a block closing token.
isBlockClose :: Char -> Bool
isBlockClose ch = ch `elem` blockClose


-- | Definition of all the characters reserved for operators
operators :: String
operators = "+-*/^&%!~=><"

-- | Definition of all the keywords in the language reserved for character purposes
keywords :: [ String ]
keywords = [ "const", "let", "fn", "rt", "if", "else", "when", "struct", "enum", "infix", "assoc", "return", "true", "false"]

-- Function that converts a string to a keyword. Gives nothing if the string is not a keyword.
toKeyword :: String -> Maybe Keyword
-- Const keyword, defines a constant in the language. May be used for inlining.
toKeyword "const" = Just Const
-- Let keyword, defines a variable in the language.
toKeyword "let" = Just Let
-- Fn keyword, defines a function definition.
toKeyword "fn" = Just Fn
-- Rt keyword, defines a function definition.
toKeyword "rt" = Just Rt
-- If keyword, defines an if statement
toKeyword "if" = Just If
-- Else keyword, defines an else clause of an if statement.
toKeyword "else" = Just Else
-- When keyword, defines a switch-case like construct. NOT IMPLEMENTED
toKeyword "when" = Just When
-- Struct keyword, defines a struct. Implemented but does not work.
toKeyword "struct" = Just Struct
-- Enum keyword, defines an enumeration of values. NOT IMPLEMENTED
toKeyword "enum" = Just Enum
-- Infix keyword, defines that a function might be called as an Infix operator, just like operators. NOT IMPLEMENTED.
toKeyword "infix" = Just Infix
-- Assoc keyword, defines that a function is associated with a struct. The function can then be called oo style. NOT IMPLEMENTED.
toKeyword "assoc" = Just Assoc
-- Return keyword, defines that the value should be returned to the callee of the current function.
toKeyword "return" = Just Return
-- While statement, defines a while loop. 
toKeyword "while" = Just While
-- For keyword, defines a Rust style for loop. NOT IMPLEMENTED.
toKeyword "for" = Just For
-- True keyword, defines the true value in the language.
toKeyword "true" = Just TrueVal
-- False keyword, defines the false value in the language
toKeyword "false" = Just FalseVal
-- If the language does not support the keyword, return Nothing.
toKeyword _ = Nothing



-- Function that checks if an character might be a operator.
isOperator :: Char -> Bool
isOperator ch = ch `elem` operators
-- Checks if a given string is a keyword.
isKeyword :: String -> Bool
isKeyword str = case toKeyword str of Nothing -> False; _ -> True;

-- Checks if a given character might be a identifier or keyword. Is used for splitting keywords.
isAcceptableIdentifier :: Char -> Bool
isAcceptableIdentifier ';' = False
isAcceptableIdentifier ':' = False
isAcceptableIdentifier ',' = False
isAcceptableIdentifier ch = not(isOperator ch) && not(isBlockOpen ch) &&  not(isBlockClose ch)

-- Lexes an operator
lexOperator :: String -> String -> [ FrogToken ]
-- If both buffers are empty, return an empty buffer.
lexOperator [] [] = []
-- Assume that if the input is empty, the given buffer contains the operator that should be returned.
lexOperator [] buffer = [ Operator buffer ]
-- As long as the character is an operator, append to the buffer.
lexOperator (x : xs) buffer | isOperator x = lexOperator xs (buffer ++ [x])
-- If the the character at the front of the input is not an operator characterm then the Operator shall be returned.
lexOperator input buffer =  Operator buffer : lexFrog input

-- Lex a digit.
lexDigit :: String -> String -> [ FrogToken ]
-- If both buffers are empty, return an empty buffer.
lexDigit [] [] = []
-- Assume that if the input is empty, the given buffer contains the digit that shall be returned.
lexDigit [] buffer 
    | '.' `elem` buffer = [ DigitLiteral (read buffer :: Float) ]
    | otherwise = [ IntegerLiteral (read buffer :: Int) ]
    
-- As long as the character is an operator, append to the buffer.
lexDigit (x: xs) buffer 
    | isDigit x || x `elem` ".,"  = lexDigit xs (buffer ++ [x])
    | '.' `elem` buffer = DigitLiteral (read buffer :: Float) : lexFrog (x : xs)
    | otherwise = IntegerLiteral (read buffer :: Int) : lexFrog (x : xs)
-- lexDigit (x: xs) buffer
    
    
    
    -- DigitLiteral (read buffer :: Float) : lexFrog (x:xs)

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

-- | Lex a comment. Places a all characters after // and before \n
lexSLComment:: String -> String -> [FrogToken] 
lexSLComment ('\n': xs) buffer = SLComment buffer : lexFrog xs
lexSLComment (x: xs) buffer = lexSLComment xs (buffer ++ [x])
-- | Lex a multiline comment. Goes on until it finds */
lexMLComment:: String -> String -> [FrogToken]
lexMLComment ('*' : '/': xs) buffer = MLComment buffer : lexFrog xs
lexMLComment (x : xs) buffer = lexMLComment xs (buffer ++ [x])
-- | Lex a multiline doc comment. Goes on until it finds */
lexDocComment :: String -> String -> [FrogToken]
lexDocComment ('*' : '/': xs) buffer = MLComment buffer : lexFrog xs
lexDocComment (x : xs) buffer = lexDocComment xs (buffer ++ [x])
-- | Lexes a string into tokens
lexFrog :: String -> [ FrogToken ]
-- | If the input buffer is empty, return empty token buffer.
lexFrog [] = []
-- | Range operator
lexFrog ('.' : '.' : xs) = Operator ".." : lexFrog xs
-- | Acces operator
lexFrog ('.' : xs) = Accessor "." : lexFrog xs

-- | Detects single line comment, begins with //
lexFrog ('/' : '/' : xs) = lexSLComment xs ""
-- | Detects multi line documentation comment.
lexFrog ('/' : '*' : '*' : xs) = lexDocComment xs ""
-- | Detects multi line comment.
lexFrog ('/' : '*' : xs) = lexMLComment xs ""

-- | Detects a string
lexFrog ('"' : xs) = lexString xs ""

-- | General implementation for single character indicatiors
lexFrog (x : xs)
    | isBlockOpen x =  BlockOpen x : lexFrog xs -- If the character is a block opening character, convert it to an block open token.
    | isBlockClose x  = BlockClose x : lexFrog xs -- If the character is a closing block character, convert it to a block close token.
    | isSpace x = lexFrog xs -- If the character is a space, skip.
    | isDigit x = lexDigit (x : xs) "" -- If the character is a digit, then try to read the rest of the digit.
    | isOperator x = lexOperator xs [x] -- If the character is an operator, try to read the rest of the operator.
    | isAcceptableIdentifier x = lexIdentifier (x : xs) "" -- If the character might be an identifier, try to read it as an identifier or keyword.
    | otherwise = Other x : lexFrog xs -- If no match is found, place it in the "other" bucket.
-- If it cannot place a character, return none.
lexFrog list = [ None ]
