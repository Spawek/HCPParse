{-# LANGUAGE FlexibleContexts #-}

module HCPParse where 

import Control.Applicative hiding ((<|>), many)
import System.Environment
import Data.Either.Unwrap
import Data.List
import Text.Parsec
import Data.Functor.Identity

-- TODO: rewrite 1) to just remove slash endlines - not split to lines - its quite useless and im doing it back in a while

-- nice links:
-- http://www.vex.net/~trebla/haskell/parsec-generally.xhtml

-- according to C++14 draft:
--    1) remove slash endlines
--    2) join whitespaces
--    3) do preprocessing
--        a) preprocessing tokenizer
--        b) ...


-- as in C++14 standard
data PPTokenType = 
    Header_name |
    Identifier |
    Pp_number | 
    Character_literal |
    User_defined_character_literal |
    String_literal |
    User_defined_string_literal |
    Preprocessing_op_or_punc |
    PP_NewLine |
    PP_AnythingElse |
    PP_Comment
    deriving (Show, Eq)

data PPToken = PPToken {
    tokenType :: PPTokenType,
    text :: String
} deriving (Eq)

instance Show PPToken where
    show (PPToken tokenType text) = "(" ++ show tokenType ++ " : " ++ show text ++ ")"

-- http://stackoverflow.com/questions/2473615/parsec-3-1-0-with-custom-token-datatype
oneOfT :: (Eq t, Show t, Stream s m t) => [t] -> ParsecT s u m t
oneOfT ts = satisfyT (`elem` ts)

noneOfT :: (Eq t, Show t, Stream s m t) => [t] -> ParsecT s u m t
noneOfT ts = satisfyT (not . (`elem` ts))

anyT :: (Show t, Stream s m t) => ParsecT s u m t
anyT = satisfyT (const True)

satisfyT :: (Show t, Stream s m t) => (t -> Bool) -> ParsecT s u m t
satisfyT p = tokenPrim showTok nextPos testTok
    where
      showTok t     = show t
      testTok t     = if p t then Just t else Nothing
      nextPos p t s = p  -- NOTE: position is not really passed

matchTokenType :: (Stream s m PPToken) => PPTokenType -> ParsecT s u m PPToken
matchTokenType expectedType = satisfyT (\x -> (tokenType x) == (expectedType)) <?> (show expectedType)

-- TODO: can be optimized for speed (don't create new token)
matchToken :: (Stream s m PPToken) => PPTokenType -> String -> ParsecT s u m PPToken
matchToken expectedType expectedText = satisfyT (== (PPToken expectedType expectedText)) <?> (show expectedType ++ " : " ++ show expectedText)

cppNondigit :: Stream s m Char => ParsecT s u m Char
cppNondigit = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

cppDigit :: Stream s m Char => ParsecT s u m Char
cppDigit = oneOf "0123456789"

identifier_digit_or_nondigit :: Stream s m Char => ParsecT s u m Char
identifier_digit_or_nondigit = cppNondigit <|> cppDigit

cppSign :: Stream s m Char => ParsecT s u m Char
cppSign = oneOf "+-"

pp_comment :: Stream s m Char => ParsecT s u m PPToken
pp_comment = do
    x <- try(string "/*") <|> try(string "//")
    return $ PPToken PP_Comment x

-- punctors which are suffix of other punctors should be at the end of list
punctors_list = ["{", "}", "[", "]", "#", "##", "(", ")", "<:", ":>", "<%", "%>", "%:%:", "%:", ";", ":", "...", "new", "delete", "?", "::", "+=", "-=", "*=", "/=", "%=", "ˆ=", "&=", "|=", "<<", ">>", ">>=", "<<=", "==", "!=", "<=", ">=", "&&", "||", "++", "--", ",", "->*", "->", ".", ".*", "+", "-", "*", "/", "%", "ˆ", "&", "|", "~", "!", "=", "<", ">", "and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq", "or", "or_eq", "xor", "xor_eq"]
preprocessing_op_or_punc2 :: Stream s m Char => [String] -> ParsecT s u m String
preprocessing_op_or_punc2 [x] = try (string x)
preprocessing_op_or_punc2 (x:xs) = try (string x) <|> (preprocessing_op_or_punc2 xs)

preprocessing_op_or_punc :: Stream s m Char => ParsecT s u m PPToken
preprocessing_op_or_punc = do
    x <- preprocessing_op_or_punc2 punctors_list
    return $ PPToken Preprocessing_op_or_punc x

header_name :: Stream s m Char => ParsecT s u m PPToken
header_name = do
    x <- h_header_name <|> q_header_name
    return $ PPToken Header_name x

h_header_name :: Stream s m Char => ParsecT s u m String
h_header_name = do
    char '<'
    name <- h_char_sequence 
    char '>'
    return $ "<" ++ name ++ ">"

q_header_name :: Stream s m Char => ParsecT s u m String
q_header_name = do
    char '\"'
    name <- q_char_sequence 
    char '\"'
    return $ "\"" ++ name ++ "\""

h_char :: Stream s m Char => ParsecT s u m Char
h_char = noneOf "\n\r>"

h_char_sequence :: Stream s m Char => ParsecT s u m String
h_char_sequence = many1 h_char

q_char :: Stream s m Char => ParsecT s u m Char
q_char = noneOf "\n\r\""

q_char_sequence :: Stream s m Char => ParsecT s u m String
q_char_sequence = many1 q_char

identifier :: Stream s m Char => ParsecT s u m PPToken
identifier = PPToken Identifier <$> do
    x <- cppNondigit
    y <- many identifier_digit_or_nondigit
    return (x:y)

pp_number :: Stream s m Char => ParsecT s u m PPToken
pp_number = PPToken Pp_number <$> do
    x <- cppDigit
    y <- many pp_number_char
    return (x:y)

pp_number_char :: Stream s m Char => ParsecT s u m Char
pp_number_char = cppDigit <|> cppNondigit <|> oneOf(".'")

character_literal :: Stream s m Char => ParsecT s u m PPToken
character_literal = PPToken Character_literal <$> do
    x <- option "" encoding_prefix
    y <- char '\''
    z <- c_char_sequence
    w <- char '\''
    return $ x ++ [y] ++ z ++ [w]

encoding_prefix :: Stream s m Char => ParsecT s u m String
encoding_prefix = try (string "u8") <|> encoding_prefix2

encoding_prefix2 :: Stream s m Char => ParsecT s u m String
encoding_prefix2 = do
    x <- oneOf "uUL"
    return [x]

c_char_sequence :: Stream s m Char => ParsecT s u m String
c_char_sequence = do
    x <- many c_char
    return $ concat x

c_char :: Stream s m Char => ParsecT s u m String
c_char = c_char_char <|> escape_sequence -- <|> universal_character_name -- NOTE: not supported
    
c_char_char :: Stream s m Char => ParsecT s u m String
c_char_char = do
    x <- noneOf ("'\\\n")
    return [x]


-- TODO change all the code to look like this one?
-- string_literal :: Stream s m Char => ParsecT s u m PPToken
-- string_literal = (PPToken String_literal) <$>
--     (string_literal_s_char_sequence <|> string_literal_raw_string)

string_literal :: Stream s m Char => ParsecT s u m PPToken
string_literal = PPToken String_literal <$> do
    x <- string_literal_s_char_sequence <|> string_literal_raw_string
    return x

string_literal_s_char_sequence :: Stream s m Char => ParsecT s u m String
string_literal_s_char_sequence = do
    x <- option "" encoding_prefix
    y <- char '"'
    z <- option "" s_char_sequence
    w <- char '"'
    return $ x ++ [y] ++ z ++ [w]

s_char_sequence :: Stream s m Char => ParsecT s u m String
s_char_sequence = do
    x <- many1 s_char
    return $ concat x

s_char :: Stream s m Char => ParsecT s u m String
s_char = s_char_char <|> escape_sequence -- <|> universal_character_name -- NOTE: not supported

s_char_char :: Stream s m Char => ParsecT s u m String
s_char_char = do
    x <- noneOf ("\"\\\n")
    return [x]

string_literal_raw_string :: Stream s m Char => ParsecT s u m String
string_literal_raw_string = do
    x <- option "" encoding_prefix
    y <- char 'R'
    z <- raw_string
    return $ x ++ [y] ++ z

raw_string :: Stream s m Char => ParsecT s u m String
raw_string = do
    x <- char '"'
    escape <- option "" d_char_sequence
    z <- char '('
    w <- option "" (r_char_sequence escape)
    q <- char ')'
    r <- string escape
    u <- char '"'
    return $ [x] ++ escape ++ [z] ++ w ++ [q] ++ r ++ [u]

r_char_sequence :: Stream s m Char => String -> (ParsecT s u m String)
r_char_sequence escape = many1 (r_char escape)

r_char :: Stream s m Char => String -> (ParsecT s u m Char)
r_char escape = do
    notFollowedBy( string( ")" ++ escape ++ "\"") )
    x <- anyToken
    return x

d_char_sequence :: Stream s m Char => ParsecT s u m String
d_char_sequence = many1 d_char

d_char :: Stream s m Char => ParsecT s u m Char
d_char = noneOf(" ()\\\t\v\f\n")

escape_sequence :: Stream s m Char => ParsecT s u m String
escape_sequence = simple_escape_sequence  -- <|> octal_escape_sequence <|> hexadecimal_escape_sequence -- NOTE: not supported (for now?)

simple_escape_sequence :: Stream s m Char => ParsecT s u m String
simple_escape_sequence = do
    x <- char '\\'
    y <- oneOf("’\"?\\abfnrtv")
    return $ x:[y]

cppLines :: Stream s m Char => ParsecT s u m String
cppLines = do
    x <- many cutSlashEndlines
    eof
    return x

cutSlashEndlines :: Stream s m Char => ParsecT s u m Char
cutSlashEndlines = try (char '\\' >> oneOf("\n\r") >> anyChar) <|> anyChar

whiteSpaceChar :: [Char]
whiteSpaceChar = "\n\r\t\v " -- NOT FROM STANDARD

consecutiveWhiteSpaceChars :: Stream s m Char => ParsecT s u m String
consecutiveWhiteSpaceChars = do
    x <- many1 (oneOf whiteSpaceChar)
    return $ (joinWhiteSpaces x)

joinWhiteSpaces :: String -> String
joinWhiteSpaces [] = []
joinWhiteSpaces x 
                | (elem '\n' x) || (elem '\r' x) = "\n"
                | otherwise    = " "

anyCharacter :: Stream s m Char => ParsecT s u m String
anyCharacter = consecutiveWhiteSpaceChars <|> (many1 (noneOf whiteSpaceChar))

joinWhitespaces :: Stream s m Char => ParsecT s u m String
joinWhitespaces = do
    x <- many anyCharacter
    eof
    return $ concat x

notEndlineWhiteSpace :: Stream s m Char => ParsecT s u m Char
notEndlineWhiteSpace = oneOf "\t\v "

ppTokenizer :: Stream s m Char => ParsecT s u m [PPToken]
ppTokenizer = do
    skipMany notEndlineWhiteSpace
    x <- many ppTokenizer2
    eof
    return x

ppNonWhite :: Stream s m Char => ParsecT s u m PPToken
ppNonWhite = PPToken PP_AnythingElse <$> do
    x <- noneOf whiteSpaceChar
    return [x]

ppNewLine :: Stream s m Char => ParsecT s u m PPToken
ppNewLine = PPToken PP_NewLine <$> do
    x <- oneOf "\n\r"
    return [x] 

ppTokenizer2 :: Stream s m Char => ParsecT s u m PPToken
ppTokenizer2 = do
    x <- try header_name <|> try identifier <|> try pp_number <|>
         try character_literal <|> try string_literal <|> try pp_comment <|>
         try preprocessing_op_or_punc <|> try ppNonWhite <|> try ppNewLine 
    skipMany notEndlineWhiteSpace
    case x of
        PPToken PP_Comment "/*" -> do
            manyTill anyChar (try (string "*/"))
            ppTokenizer2
        PPToken PP_Comment "//" -> do
            manyTill anyChar (try ppNewLine)
            return $ PPToken PP_NewLine "\n"
        PPToken _ _ -> return x

restringifyTokens :: [PPToken] -> [String]
restringifyTokens = map text

ppTokenize :: Stream s Identity Char => s -> Either ParseError [PPToken]
ppTokenize rawText = do
    parsedLines <- parse cppLines "((UNKNOWN CPPLINES))" rawText
    preparedForPp <- parse joinWhitespaces "((WHITESPACE JOIN))" parsedLines
    tokens <- parse ppTokenizer "((UNKNOWN PREPROC))" preparedForPp
    return tokens

-- ppDirective jest na poczatku lini

-- PPFile = opt PPGroup
-- PPGroup = [PPGroupPart] - at least one
-- so PPFile = [PPGroupPart] -- 0 or more

type PPFile = [PPGroupPart]

data PPGroupPart = PPGroupPart{
    groupType :: PPGroupPartType,
    groupTokens :: [PPToken],
    subGroups :: [PPGroupPart] -- TODO: shouln't it be a part of If_group?
}  deriving (Eq, Show)

data PPGroupPartType =
    If_section {if_group :: PPGroupPart, endif_line :: PPGroupPart}|
    Control_line |
    Text_line |
    Non_directive |
    If_group | -- NOTE: maybe it shouldn't be here
    Endif_line  -- NOTE: maybe it shouldn't be here
    deriving (Eq, Show)

ppGroup :: Stream s m PPToken => ParsecT s u m PPGroupPart
ppGroup = try (ifSection) -- temporary only this

ifSection :: Stream s m PPToken => ParsecT s u m PPGroupPart
ifSection = do
    x <- ifGroup
    y <- endifLine
    return $ PPGroupPart (If_section x y) [] []

endifLine :: Stream s m PPToken => ParsecT s u m PPGroupPart
endifLine = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "endif"
    matchTokenType PP_NewLine
    return $ PPGroupPart Endif_line [x, y] []

-- NOTE: only #ifndef supported for now
ifGroup :: Stream s m PPToken => ParsecT s u m PPGroupPart
ifGroup = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "ifndef"
    identifier <- matchTokenType Identifier
    matchTokenType PP_NewLine
    subGroups <- many ppGroup
    return $ PPGroupPart If_group [x, y, identifier] subGroups

