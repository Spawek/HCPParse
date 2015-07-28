import Text.ParserCombinators.Parsec
import System.Environment
--import Text.Parsec.String

-- according to C++14 draft:
--    1) remove slash endlines
--    2) do preprocessing
--        a) preprocessing tokenizer
--        b) ...

-- NOTE: GenParser String st String - "st" means state actually - I can use of it somehow

data RawLine = RawLine {
    lineContent :: String,
    lineNo :: Int
} deriving (Show)

data PreprocessorDefine = PreprocessorDefine {
    from :: String,
    to :: String
}

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

data PreprocessingLine = PreprocessingLine{
    tokens :: [PreprocessingToken],
    line :: RawLine
} deriving (Show)

-- from C++14 standard
cppNondigit :: GenParser Char st Char
cppNondigit = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

cppDigit :: GenParser Char st Char
cppDigit = oneOf "0123456789"

identifier_digit_or_nondigit :: GenParser Char st Char
identifier_digit_or_nondigit = cppNondigit <|> cppDigit

cppSign :: GenParser Char st Char
cppSign = oneOf "+-"

preprocessing_op_or_punc :: GenParser Char st String
preprocessing_op_or_punc = string "{" <|> string "}" <|> string "[" <|> string "]" <|> string "#" <|> string "##" <|> string "(" <|> string ")" <|>
    string "<:" <|> string ":>" <|> string "<%" <|> string "%>" <|> string "%:" <|> string "%:%:" <|> string ";" <|> string ":" <|> string "..." <|>
    string "new" <|> string "delete" <|> string "?" <|> string "::" <|> string "." <|> string ".*" <|>
    string "+" <|> string "-" <|> string "*" <|> string "/" <|> string "%" <|> string "ˆ" <|> string "&" <|> string "|" <|> string "~" <|>
    string "!" <|> string "=" <|> string "<" <|> string ">" <|> string "+=" <|> string "-=" <|> string "*=" <|> string "/=" <|>  string "%=" <|>
    string "ˆ=" <|> string "&=" <|> string "|=" <|> string "<<" <|> string ">>" <|> string ">>=" <|> string "<<=" <|> string "==" <|> string "!=" <|>
    string "<=" <|> string ">=" <|> string "&&" <|> string "||" <|> string "++" <|> string "--" <|> string "," <|> string "->*" <|> string "->" <|>
    string "and" <|> string "and_eq" <|> string "bitand" <|> string "bitor" <|> string "compl" <|> string "not" <|> string "not_eq "<|> 
    string "or" <|> string "or_eq" <|> string "xor" <|> string "xor_eq"

header_name :: GenParser Char st PreprocessingToken
header_name = do
    x <- h_header_name <|> q_header_name
    return (PreprocessingToken Header_name x)

h_header_name :: GenParser Char st String
h_header_name = do
    char '<'
    name <- h_char_sequence 
    char '>'
    return name

q_header_name :: GenParser Char st String
q_header_name = do
    char '\"'
    name <- q_char_sequence 
    char '\"'
    return name

h_char :: GenParser Char st Char
h_char = noneOf "\n\r>"

h_char_sequence :: GenParser Char st String
h_char_sequence = many1 h_char

q_char :: GenParser Char st Char
q_char = noneOf "\n\r\""

q_char_sequence :: GenParser Char st String
q_char_sequence = many1 q_char

identifier :: GenParser Char st PreprocessingToken
identifier = do
    x <- cppNondigit
    y <- many identifier_digit_or_nondigit
    return (PreprocessingToken Identifier (x:y))

pp_number :: GenParser Char st PreprocessingToken
pp_number = do
    x <- cppDigit
    y <- many pp_number_char
    return (PreprocessingToken Pp_number (x:y))

pp_number_char :: GenParser Char st Char
pp_number_char = cppDigit <|> cppNondigit <|> oneOf(".'")

character_literal :: GenParser Char st PreprocessingToken
character_literal = do
    x <- option "" encoding_prefix
    y <- char '\''
    z <- c_char_sequence
    w <- char '\''
    return (PreprocessingToken Character_literal (x ++ [y] ++ z ++ [w]))

encoding_prefix :: GenParser Char st String
encoding_prefix = try (string "u8") <|> encoding_prefix2

encoding_prefix2 :: GenParser Char st String
encoding_prefix2 = do
    x <- oneOf "uUL"
    return [x]

c_char_sequence :: GenParser Char st String
c_char_sequence = do
    x <- many c_char
    return (concat x)

c_char :: GenParser Char st String
c_char = c_char_char <|> escape_sequence -- <|> universal_character_name -- NOTE: not supported
    
c_char_char :: GenParser Char st String
c_char_char = do
    x <- noneOf ("'\\\n")
    return [x]

string_literal :: GenParser Char st PreprocessingToken
string_literal = do
    x <- (string_literal_s_char_sequence <|> string_literal_raw_string)
    return (PreprocessingToken String_literal x)

string_literal_s_char_sequence :: GenParser Char st String
string_literal_s_char_sequence = do
    x <- option "" encoding_prefix
    y <- char '"'
    z <- option "" s_char_sequence
    w <- char '"'
    return (x ++ [y] ++ z ++ [w])

s_char_sequence :: GenParser Char st String
s_char_sequence = do
    x <- many1 s_char
    return (concat x)

s_char :: GenParser Char st String
s_char = s_char_char <|> escape_sequence -- <|> universal_character_name -- NOTE: not supported

s_char_char :: GenParser Char st String
s_char_char = do
    x <- noneOf ("\"\\\n")
    return [x]

string_literal_raw_string :: GenParser Char st String
string_literal_raw_string = do
    x <- option "" encoding_prefix
    y <- char 'R'
    z <- raw_string
    return (x ++ [y] ++ z)

raw_string :: GenParser Char st String
raw_string = do
    x <- char '"'
    escape <- option "" d_char_sequence
    z <- char '('
    w <- option "" (r_char_sequence escape)
    q <- char ')'
    r <- string escape
    u <- char '"'
    return ([x] ++ escape ++ [z] ++ w ++ [q] ++ r ++ [u])

r_char_sequence :: String -> (GenParser Char st String)
r_char_sequence escape = many1 (r_char escape)

r_char :: String -> (GenParser Char st Char)
r_char escape = do
    notFollowedBy( string( ")" ++ escape ++ "\"") )
    x <- anyToken
    return x

d_char_sequence :: GenParser Char st String
d_char_sequence = many1 d_char

d_char :: GenParser Char st Char
d_char = noneOf(" ()\\\t\v\f\n")

escape_sequence :: GenParser Char st String
escape_sequence = simple_escape_sequence  -- <|> octal_escape_sequence <|> hexadecimal_escape_sequence -- NOTE: not supported (for now?)

simple_escape_sequence :: GenParser Char st String
simple_escape_sequence = do
    x <- char '\\'
    y <- oneOf("’\"?\\abfnrtv")
    return (x:[y])

preprocessorTokenize :: Either ParseError [RawLine] -> Either ParseError [PreprocessingLine]
preprocessorTokenize (Left err) = Left err
preprocessorTokenize (Right parsed) = Right $ map preprocessorTokenizeLine parsed

preprocessorTokenizeLine :: RawLine -> PreprocessingLine
preprocessorTokenizeLine line = PreprocessingLine [] line --DUMMY! TODO: add parser here


--applyDefines :: Either ParseError [RawLine] -> Either ParseError [RawLine]
--applyDefines (Left err) = Left err
--applyDefines (Right parsed) = Right $ applyDefines2 [] parsed []

--applyDefines2 :: [PreprocessorDefine] -> [RawLine] -> [RawLine]
--applyDefines2 _ [] x = x
--applyDefines2 defines [nextInput:restInput] out = applyDefines2 newDefinesList xs newOutList
--    where newLine = useDefines defines nextInput
--          newOutList = [newLine:out]
--          newDefinesList = appendDefine nextInput defines

--useDefines ::

--defines without parameters only

--TODO: change it to monad use
makeRawLines :: Either ParseError [String] -> Either ParseError [RawLine]
makeRawLines (Left err) = Left err
makeRawLines (Right parsed) = Right $ makeRawLines2 1 parsed

--BUG: lines with \\\n are not counted
makeRawLines2 :: Int -> [String] -> [RawLine]
makeRawLines2 _ [] = []
makeRawLines2 lineNo (x:xs) = ((RawLine x lineNo):(makeRawLines2 (lineNo + 1) xs))

cppLines = endBy notEOL eolChar
eolChar = oneOf ("\n\r")
notEOL = many notEOLHelp
notEOLHelp = try (char '\\' >> oneOf("\n\r") >> anyChar) <|> (noneOf "\n\r") -- TODO: lines should be counted here - somehow


main :: IO()
main = readFile "testIn.cpp" >>= print . makeRawLines . parse cppLines "((UNKNOWN))"
--main = do
--    x <- readFile "testIn.cpp" --TODO: zapytac na stacku?
--    return x >>= print . makeRawLines . parse cppLines "((UNKNOWN))")

