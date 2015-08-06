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
    Preprocessing_op_or_punc |
    PP_WhiteSpace |
    PP_AnythingElse
    deriving (Show)

data PreprocessingToken = PreprocessingToken {
    tokenType :: PreprocessingTokenType,
    text :: String
} deriving (Show)

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

cppLines = endBy notEOL eolChar
eolChar = oneOf ("\n\r")
notEOL = many notEOLHelp
notEOLHelp = try (char '\\' >> oneOf("\n\r") >> anyChar) <|> (noneOf "\n\r")

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

ppTokenizer :: Parser [PreprocessingToken]
ppTokenizer = do
    x <- many ppTokenizer2
    eof
    return x

ppNonWhite :: Parser PreprocessingToken
ppNonWhite = do
    x <- noneOf whitespaceChar
    return $ PreprocessingToken PP_AnythingElse [x]

ppWhite :: Parser PreprocessingToken
ppWhite = do
    x <- oneOf whitespaceChar
    return $ PreprocessingToken PP_WhiteSpace [x]

ppTokenizer2 :: Parser PreprocessingToken
ppTokenizer2 = try header_name <|> try identifier <|> try pp_number <|>
               try character_literal <|> try string_literal <|>
               try preprocessing_op_or_punc <|> try ppNonWhite <|>
               try ppWhite

main :: IO()
main = do
    rawText <- readFile "testIn.cpp"
    putStr "\nRAW TEXT BEGIN\n"
    putStr rawText
    putStr "\nRAW TEXT END\n"
    eitherParsedLines <- return $ parse cppLines "((UNKNOWN))" rawText
    case eitherParsedLines of
        Left err -> print $ "line parse error: " ++ show err
        Right parsedLines -> do
            print parsedLines
            eitherPreparedForPp <- return $ parse joinWhitespaces "((WHITESPACE JOIN))" (concatWith "\n" parsedLines) -- NOTE: line endings are lost here
            case eitherPreparedForPp of
                Left err -> print $ "prepare for preprocessing failed: " ++ show err
                Right preparedForPp -> do
                    putStr "\nPRE PREPROCESSING BEGIN\n"
                    putStr preparedForPp
                    putStr "\nPRE PREPROCESSING END\n"
                    tokens <- return $ parse ppTokenizer "((UNKNOWN PREPROC))" preparedForPp
                    print tokens
