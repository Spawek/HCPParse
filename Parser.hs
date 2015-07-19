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

digit :: GenParser Char st Char
digit = oneOf "0123456789"

--aaaeol :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m String
--aaaeol = string "\n" <|> string "\n\r"

--preprocessing_op_orPunc :: GenParser String st Char
--preprocessing_op_orPunc :: ParsecT s u m String
preprocessing_op_orPunc :: GenParser Char st String
preprocessing_op_orPunc = string "{" <|> string "}" <|> string "[" <|> string "]" <|> string "#" <|> string "##" <|> string "(" <|> string ")" <|>
    string "<:" <|> string ":>" <|> string "<%" <|> string "%>" <|> string "%:" <|> string "%:%:" <|> string ";" <|> string ":" <|> string "..." <|>
    string "new" <|> string "delete" <|> string "?" <|> string "::" <|> string "." <|> string ".*" <|>
    string "+" <|> string "-" <|> string "*" <|> string "/" <|> string "%" <|> string "ˆ" <|> string "&" <|> string "|" <|> string "~" <|>
    string "!" <|> string "=" <|> string "<" <|> string ">" <|> string "+=" <|> string "-=" <|> string "*=" <|> string "/=" <|>  string "%=" <|>
    string "ˆ=" <|> string "&=" <|> string "|=" <|> string "<<" <|> string ">>" <|> string ">>=" <|> string "<<=" <|> string "==" <|> string "!=" <|>
    string "<=" <|> string ">=" <|> string "&&" <|> string "||" <|> string "++" <|> string "--" <|> string "," <|> string "->*" <|> string "->" <|>
    string "and" <|> string "and_eq" <|> string "bitand" <|> string "bitor" <|> string "compl" <|> string "not" <|> string "not_eq "<|> 
    string "or" <|> string "or_eq" <|> string "xor" <|> string "xor_eq"

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


main = readFile "testIn.cpp" >>= print . makeRawLines . parse cppLines "((UNKNOWN))"
--t2 = parse aaaeol "testestes"

