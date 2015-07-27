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

-- HEADER
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
    y <- many identifier_any
    return (PreprocessingToken Identifier (x:y))

identifier_any :: GenParser Char st Char
identifier_any = cppNondigit <|> cppDigit

-- NOTE: universal-character-name is not supported (as I wanna kill every person who is using it)
identifier_nondigit :: GenParser Char st PreprocessingToken
identifier_nondigit = do
    x <- cppNondigit 
    return (PreprocessingToken Identifier [x])

identifier_digit :: GenParser Char st PreprocessingToken
identifier_digit = do
    x <- identifier
    y <- cppDigit
    return (PreprocessingToken Identifier ((text x) ++ [y]))

-- PP_NUMBER - zapetla sie przy blednym!
pp_number :: GenParser Char st PreprocessingToken
pp_number = pp_digit <|>
    pp_dot_digit  <|>
    pp_number_digit <|>
    pp_number_identifier_nondigit <|>
    pp_number_quote_digit <|>
    pp_number_quote_nondigit <|>
    pp_number_e_sign <|>
    pp_number_E_sign <|>
    pp_number_dot

pp_digit :: GenParser Char st PreprocessingToken
pp_digit = do
    x <- cppDigit
    return (PreprocessingToken Pp_number [x])

pp_dot_digit :: GenParser Char st PreprocessingToken
pp_dot_digit = do
    x <- char '.'
    y <- cppDigit
    return (PreprocessingToken Pp_number (x:[y]))

pp_number_digit :: GenParser Char st PreprocessingToken
pp_number_digit = do
    x <- pp_number
    y <- cppDigit
    return (PreprocessingToken Pp_number ((text x) ++ [y]))

pp_number_identifier_nondigit :: GenParser Char st PreprocessingToken
pp_number_identifier_nondigit = do
    x <- pp_number
    y <- identifier_nondigit
    return (PreprocessingToken Pp_number ((text x) ++ (text y)))

pp_number_quote_digit :: GenParser Char st PreprocessingToken
pp_number_quote_digit = do
    x <- pp_number
    y <- char '\''
    z <- cppDigit
    return (PreprocessingToken Pp_number ((text x) ++ [y] ++ [z]))

pp_number_quote_nondigit :: GenParser Char st PreprocessingToken
pp_number_quote_nondigit = do
    x <- pp_number
    y <- char '\''
    z <- cppNondigit
    return (PreprocessingToken Pp_number ((text x) ++ [y] ++ [z]))

pp_number_e_sign :: GenParser Char st PreprocessingToken
pp_number_e_sign = do
    x <- pp_number
    y <- char 'e'
    z <- cppSign
    return (PreprocessingToken Pp_number ((text x) ++ [y] ++ [z]))

pp_number_E_sign :: GenParser Char st PreprocessingToken
pp_number_E_sign = do
    x <- pp_number
    y <- char 'E'
    z <- cppSign
    return (PreprocessingToken Pp_number ((text x) ++ [y] ++ [z]))

pp_number_dot :: GenParser Char st PreprocessingToken
pp_number_dot = do
    x <- pp_number
    y <- char '.'
    return (PreprocessingToken Pp_number ((text x) ++ [y]))

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

