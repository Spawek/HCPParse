import Text.ParserCombinators.Parsec
import System.Environment
import Data.Either.Unwrap
import Data.List
import Lib.Util(concatWith)
--import Text.Parsec.String

-- TODO: rewrite 1) to just remove slash endlines - not split to lines - its quite useless and im doing it back in a while

-- nice links:
-- http://www.vex.net/~trebla/haskell/parsec-generally.xhtml

-- according to C++14 draft:
--    1) remove slash endlines
--    2) join whitespaces
--    3) do preprocessing
--        a) preprocessing tokenizer
--        b) ...

-- NOTE: GenParser String st String - "st" means state actually - I can use of it somehow

--data RawLine = RawLine {
--    lineContent :: String,
--    lineNo :: Int,
--    col :: Int
--} deriving (Show)

--update_pos :: SourcePos -> RawLine -> [RawLine] -> SourcePos
--update_pos pos _ _ = pos

-- as in C++14 standard
data PreprocessingTokenType = 
    Header_name |
    Identifier |
    Pp_number | 
    Character_literal |
    User_defined_character_literal |
    String_literal |
    User_defined_string_literal |
    Preprocessing_op_or_punc
    deriving (Show)

data PreprocessingToken = PreprocessingToken {
    tokenType :: PreprocessingTokenType,
    text :: String
} deriving (Show)

--data PreprocessingLine = PreprocessingLine{
--    tokens :: [PreprocessingToken],
--    line :: RawLine
--} deriving (Show)

-- from C++14 standard
cppNondigit :: Parser Char
cppNondigit = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

cppDigit :: Parser Char
cppDigit = oneOf "0123456789"

identifier_digit_or_nondigit :: Parser Char
identifier_digit_or_nondigit = cppNondigit <|> cppDigit

cppSign :: Parser Char
cppSign = oneOf "+-"

preprocessing_op_or_punc :: Parser PreprocessingToken
preprocessing_op_or_punc = do
    x <- string "{" <|> string "}" <|> string "[" <|> string "]" <|> string "#" <|> string "##" <|> string "(" <|> string ")" <|>
        string "<:" <|> string ":>" <|> string "<%" <|> string "%>" <|> string "%:" <|> string "%:%:" <|> string ";" <|> string ":" <|> string "..." <|>
        string "new" <|> string "delete" <|> string "?" <|> string "::" <|> string "." <|> string ".*" <|>
        string "+" <|> string "-" <|> string "*" <|> string "/" <|> string "%" <|> string "ˆ" <|> string "&" <|> string "|" <|> string "~" <|>
        string "!" <|> string "=" <|> string "<" <|> string ">" <|> string "+=" <|> string "-=" <|> string "*=" <|> string "/=" <|>  string "%=" <|>
        string "ˆ=" <|> string "&=" <|> string "|=" <|> string "<<" <|> string ">>" <|> string ">>=" <|> string "<<=" <|> string "==" <|> string "!=" <|>
        string "<=" <|> string ">=" <|> string "&&" <|> string "||" <|> string "++" <|> string "--" <|> string "," <|> string "->*" <|> string "->" <|>
        string "and" <|> string "and_eq" <|> string "bitand" <|> string "bitor" <|> string "compl" <|> string "not" <|> string "not_eq "<|> 
        string "or" <|> string "or_eq" <|> string "xor" <|> string "xor_eq"
    return (PreprocessingToken Preprocessing_op_or_punc x)

header_name :: Parser PreprocessingToken
header_name = do
    x <- h_header_name <|> q_header_name
    return $ PreprocessingToken Header_name x

h_header_name :: Parser String
h_header_name = do
    char '<'
    name <- h_char_sequence 
    char '>'
    return name

q_header_name :: Parser String
q_header_name = do
    char '\"'
    name <- q_char_sequence 
    char '\"'
    return name

h_char :: Parser Char
h_char = noneOf "\n\r>"

h_char_sequence :: Parser String
h_char_sequence = many1 h_char

q_char :: Parser Char
q_char = noneOf "\n\r\""

q_char_sequence :: Parser String
q_char_sequence = many1 q_char

identifier :: Parser PreprocessingToken
identifier = do
    x <- cppNondigit
    y <- many identifier_digit_or_nondigit
    return $ PreprocessingToken Identifier (x:y)

pp_number :: Parser PreprocessingToken
pp_number = do
    x <- cppDigit
    y <- many pp_number_char
    return $ PreprocessingToken Pp_number (x:y)

pp_number_char :: Parser Char
pp_number_char = cppDigit <|> cppNondigit <|> oneOf(".'")

character_literal :: Parser PreprocessingToken
character_literal = do
    x <- option "" encoding_prefix
    y <- char '\''
    z <- c_char_sequence
    w <- char '\''
    return $ PreprocessingToken Character_literal (x ++ [y] ++ z ++ [w])

encoding_prefix :: Parser String
encoding_prefix = try (string "u8") <|> encoding_prefix2

encoding_prefix2 :: Parser String
encoding_prefix2 = do
    x <- oneOf "uUL"
    return [x]

c_char_sequence :: Parser String
c_char_sequence = do
    x <- many c_char
    return $ concat x

c_char :: Parser String
c_char = c_char_char <|> escape_sequence -- <|> universal_character_name -- NOTE: not supported
    
c_char_char :: Parser String
c_char_char = do
    x <- noneOf ("'\\\n")
    return [x]

string_literal :: Parser PreprocessingToken
string_literal = do
    x <- (string_literal_s_char_sequence <|> string_literal_raw_string)
    return $ PreprocessingToken String_literal x

string_literal_s_char_sequence :: Parser String
string_literal_s_char_sequence = do
    x <- option "" encoding_prefix
    y <- char '"'
    z <- option "" s_char_sequence
    w <- char '"'
    return $ x ++ [y] ++ z ++ [w]

s_char_sequence :: Parser String
s_char_sequence = do
    x <- many1 s_char
    return $ concat x

s_char :: Parser String
s_char = s_char_char <|> escape_sequence -- <|> universal_character_name -- NOTE: not supported

s_char_char :: Parser String
s_char_char = do
    x <- noneOf ("\"\\\n")
    return [x]

string_literal_raw_string :: Parser String
string_literal_raw_string = do
    x <- option "" encoding_prefix
    y <- char 'R'
    z <- raw_string
    return $ x ++ [y] ++ z

raw_string :: Parser String
raw_string = do
    x <- char '"'
    escape <- option "" d_char_sequence
    z <- char '('
    w <- option "" (r_char_sequence escape)
    q <- char ')'
    r <- string escape
    u <- char '"'
    return $ [x] ++ escape ++ [z] ++ w ++ [q] ++ r ++ [u]

r_char_sequence :: String -> (Parser String)
r_char_sequence escape = many1 (r_char escape)

r_char :: String -> (Parser Char)
r_char escape = do
    notFollowedBy( string( ")" ++ escape ++ "\"") )
    x <- anyToken
    return x

d_char_sequence :: Parser String
d_char_sequence = many1 d_char

d_char :: Parser Char
d_char = noneOf(" ()\\\t\v\f\n")

escape_sequence :: Parser String
escape_sequence = simple_escape_sequence  -- <|> octal_escape_sequence <|> hexadecimal_escape_sequence -- NOTE: not supported (for now?)

simple_escape_sequence :: Parser String
simple_escape_sequence = do
    x <- char '\\'
    y <- oneOf("’\"?\\abfnrtv")
    return $ x:[y]

--rawLineGetter :: GenParser RawLine st String
--rawLineGetter = do
--    x <- lineContent
--    return "dsa"

-- FROM: http://www.vex.net/~trebla/haskell/parsec-generally.xhtml
--pRawLine = tokenPrim show update_pos get_num where
--  get_num RawLine{lineContent=x} = Just x
--  get_num _ = Nothing

--update_pos :: SourcePos -> RawLine -> [RawLine] -> SourcePos
--update_pos pos _ (tok:_) = setSourceLine (setSourceColumn pos (col tok)) (lin tok)
--update_pos pos _ [] = pos



--preprocessorTokenize :: Parser PreprocessingLine
--preprocessorTokenize = do
--    token <- header_name -- . pRawLine
--    return (PreprocessingLine [] (RawLine "test" 123 0))

--preprocessorTokenize :: Either ParseError [RawLine] -> Either ParseError [PreprocessingLine]
--preprocessorTokenize (Left err) = Left err
--preprocessorTokenize (Right parsed) = Right $ map preprocessorTokenizeLine parsed

--preprocessorTokenizeLine :: RawLine -> PreprocessingLine
--preprocessorTokenizeLine line = PreprocessingLine preprocessingTokens line --DUMMY! TODO: add parser here
--    where preprocessingTokens = getPreprocessingTokens line

--getPreprocessingTokens :: RawLine -> Either ParseError [PreprocessingToken]
--getPreprocessingTokens line = 

--applyDefines :: Either ParseError [RawLine] -> Either ParseError [RawLine]
--applyDefines (Left err) = Left err
--applyDefines (Right parsed) = Right $ applyDefines2 [] parsed []

--applyDefines2 :: [PreprocessorDefine] -> [RawLine] -> [RawLine]
--applyDefines2 _ [] x = x
--applyDefines2 defines [nextInput:restInput] out = applyDefines2 newDefinesList xs newOutList
--    where newLine = useDefines defines nextInput
--          newOutList = [newLine:out]
--          newDefinesList = appendDefine nextInput defines

cppLines = endBy notEOL eolChar
eolChar = oneOf ("\n\r")
notEOL = many notEOLHelp
notEOLHelp = try (char '\\' >> oneOf("\n\r") >> anyChar) <|> (noneOf "\n\r") -- TODO: lines should be counted here - somehow

whitespaceChar = "\n\r\t\v " -- NOT FROM STANDARD

anyCharacter :: Parser String
anyCharacter = do
    white <- many (oneOf whitespaceChar)
    nonWhite <- many1 (noneOf whitespaceChar)
    return $ (changeWhiteSpaces white) ++ nonWhite

changeWhiteSpaces :: String -> String
changeWhiteSpaces [] = []
changeWhiteSpaces x 
                | elem '\n' x  = "\n"
                | otherwise    = " "

joinWhitespaces :: Parser String
joinWhitespaces = do
    x <- many anyCharacter
    eof
    return $ concat x

preprocessorTokenizer :: Parser [PreprocessingToken]
preprocessorTokenizer = do
    return [] -- DUMMY

main :: IO()
main = do
    rawText <- readFile "testIn.cpp"
    putStr "\nRAW TEXT BEGIN\n"
    putStr rawText
    putStr "\nRAW TEXT END\n"
    parsedLines <- return $ parse cppLines "((UNKNOWN))" rawText
    if (isLeft parsedLines) then
        print "line parse error"
    else do
        print parsedLines
        preparedForPreprocess <- return $ parse joinWhitespaces "((WHITESPACE JOIN))" (concatWith "\n" (fromRight parsedLines)) -- NOTE: line endings are lost here
        if (isLeft preparedForPreprocess) then
            print "prepare for preprocessing failed"
        else do
            putStr "\nPRE PREPROCESSING BEGIN\n"
            putStr $ fromRight preparedForPreprocess
            putStr "\nPRE PREPROCESSING END\n"
            tokens <- return $ parse preprocessorTokenizer "((UNKNOWN PREPROC))" (fromRight preparedForPreprocess)
            print tokens
    --print parsedLines
    --else do
    --preprocessingTokens <- return $ parse preprocessorTokenizer "((UNKNOWN PREPROC))" parsedLines

    
