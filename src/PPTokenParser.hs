{-# LANGUAGE FlexibleContexts #-}

module PPTokenParser where 

import Text.Parsec
import Tokenizer
import Control.Applicative hiding ((<|>), many)

data PPFile = PPFile [PPGroupPart] deriving (Eq)

data PPGroupPart = PPGroupPart{
    groupType :: PPGroupPartType,
    groupTokens :: [PPToken]
}  deriving (Eq)

data PPGroupPartType =
    If_section {if_group :: PPGroupPart} |
    Control_line {control_lineType :: ControlLineType} |
    Text_line |
    Non_directive |
    If_group {if_groupType :: IfGroupType, subGroups :: [PPGroupPart]} |
    Endif_line
    deriving (Eq)

data IfGroupType =
    Ifndef |
    Ifdef |
    If
    deriving (Show, Eq)

data MacroDefinition = MacroDefinition {
    macroName :: PPToken,
    paramsNo :: Int,
    replacement_list :: [PPToken]
} deriving (Show, Eq)

data ControlLineType =
    Include |
    Define {definiton :: MacroDefinition}
    deriving (Show, Eq)

showPPGroupPartsList :: [PPGroupPart] -> String
showPPGroupPartsList [] = ""
showPPGroupPartsList (x:xs) = show x ++ "\n\n" ++ showPPGroupPartsList xs

instance Show PPFile where
    show (PPFile x) = showPPGroupPartsList x

instance Show PPGroupPart where
    show (PPGroupPart groupType groupTokens) = "(" ++ show groupType ++ " : " ++ show groupTokens ++ ")\n"

instance Show PPGroupPartType where
    show (If_section if_group) = show if_group ++ "\n"
    show (Control_line control_lineType) = show control_lineType
    show Text_line = "Text"
    show Non_directive = "Non_directive"
    show (If_group t subGroups) = "\n" ++ show t ++ " :\n" ++ show subGroups
    show Endif_line = "Endif"

-- http://stackoverflow.com/questions/2473615/parsec-3-1-0-with-custom-token-datatype
-- NOTE: not sure if s (stream) had to be changed to [PPToken], but didn't see any way to use it in Pos update in the other way
oneOfT :: (Stream [PPToken] m PPToken) => [PPToken] -> ParsecT [PPToken] u m PPToken
oneOfT ts = satisfyT (`elem` ts)

noneOfT :: (Stream [PPToken] m PPToken) => [PPToken] -> ParsecT [PPToken] u m PPToken
noneOfT ts = satisfyT (not . (`elem` ts))

anyT :: (Stream [PPToken] m PPToken) => ParsecT [PPToken] u m PPToken
anyT = satisfyT (const True)

satisfyT :: (Stream [PPToken] m PPToken) => (PPToken -> Bool) -> ParsecT [PPToken] u m PPToken
satisfyT p = tokenPrim showTok nextPos testTok
    where
        showTok t       = show t
        testTok t       = if p t then Just t else Nothing
        nextPos pos (PPToken tokenType _) s = case tokenType of
            PP_NewLine -> setSourceColumn (setSourceLine pos ((sourceLine pos) + 1)) 0
            _ -> setSourceColumn pos ((sourceColumn pos) + 1)

noneOfTokenTypes :: (Stream [PPToken] m PPToken) => [PPTokenType] -> ParsecT [PPToken] u m PPToken
noneOfTokenTypes typeList = satisfyT (\x -> (not (elem (tokenType x) typeList))) <?> "(token with type different than: " ++ show typeList ++ ")"

matchTokenType :: (Stream [PPToken] m PPToken) => PPTokenType -> ParsecT [PPToken] u m PPToken
matchTokenType expectedType = satisfyT (\x -> (tokenType x) == (expectedType)) <?> (show expectedType)

-- TODO: can be optimized for speed (don't create new token)
matchToken :: (Stream [PPToken] m PPToken) => PPTokenType -> String -> ParsecT [PPToken] u m PPToken
matchToken expectedType expectedText = satisfyT (== (PPToken expectedType expectedText)) <?> "(" ++ (show expectedType ++ " : " ++ show expectedText) ++ ")"

ppGroups :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m [PPGroupPart]
ppGroups = do
    x <- many ppGroup
    eof
    return x

ppGroup :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
ppGroup = try (ifSection) <|> try (controlSection) <|> try (textLine)

ifSection :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
ifSection = do
    x <- ifGroup
    endifLine
    return $ PPGroupPart (If_section x) []

endifLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
endifLine = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "endif"
    matchTokenType PP_NewLine
    return $ PPGroupPart Endif_line [x, y]

ifndef :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m IfGroupType
ifndef = do
    matchToken Identifier "ifndef"
    return Ifndef 

ifdef :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m IfGroupType
ifdef = do
    matchToken Identifier "ifdef"
    return Ifdef 

ifDirective :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m IfGroupType
ifDirective = do
    matchToken Identifier "if"
    return If 

ifGroup :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
ifGroup = do
    matchToken Preprocessing_op_or_punc "#"
    t <- try(ifndef) <|> try(ifdef) <|> try(ifDirective)
    identifiers <- many1 (matchTokenType Identifier)
    matchTokenType PP_NewLine
    subGroups <- many ppGroup
    return $ PPGroupPart (If_group t subGroups) identifiers

controlSection :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
controlSection = try (includeLine) <|> try (defineLine)

includeLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
includeLine = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "include"
    z <- matchTokenType Header_name -- NOTE: C++ standard allows using macros for generating header names - it is not supported here
    matchTokenType PP_NewLine
    return $ PPGroupPart (Control_line Include) [z]

defineLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
defineLine = do
    matchToken Preprocessing_op_or_punc "#"
    matchToken Identifier "define"
    try argDefine <|> try noArgDefine

noArgDefine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
noArgDefine = do
    identifier <- matchTokenType Identifier
    replacement_list <- many $ noneOfTokenTypes [PP_NewLine]
    matchTokenType PP_NewLine
    return $ PPGroupPart (Control_line (Define (MacroDefinition identifier 0 replacement_list))) []

macroParameter :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPToken
macroParameter = matchTokenType Identifier

nextMacroParameter :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPToken
nextMacroParameter = do
    matchToken Preprocessing_op_or_punc ","
    param <- macroParameter
    return param

macroParameters :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m [PPToken]
macroParameters = do
    matchToken Preprocessing_op_or_punc "("
    firstParam <- macroParameter
    nextParams <- many nextMacroParameter
    matchToken Preprocessing_op_or_punc ")"
    return $ firstParam:nextParams

applyParamsToReplacementList :: [PPToken] -> [PPToken] -> [PPToken]
applyParamsToReplacementList = applyParamsToReplacementList2 0

applyParamsToReplacementList2 :: Int -> [PPToken] -> [PPToken] -> [PPToken]
applyParamsToReplacementList2 _ [] replacement_list = replacement_list
applyParamsToReplacementList2 no (x:xs) replacement_list = applyParamsToReplacementList2 (no+1) xs fixedList
    where fixedList = map (\t -> if x == t then PPToken (PP_MacroParameter no) [] else x) replacement_list

-- FROM: http://stackoverflow.com/questions/31036474/haskell-checking-if-all-list-elements-are-unique
allDifferent :: (Eq a) => [a] -> Bool -- TODO: move it to libs
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

argDefine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
argDefine = do
    identifier <- matchTokenType Identifier
    parameters <- macroParameters
    if not $ allDifferent parameters
    then unexpected $ "macro parameters are not unique: " ++ show parameters
    else do
        replacement_list <- many (noneOfTokenTypes [PP_NewLine])
        matchTokenType PP_NewLine
        return $ PPGroupPart (Control_line (Define (MacroDefinition identifier (length parameters) replacement_list))) []

emptyTextLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
emptyTextLine = do
    x <- matchTokenType PP_NewLine
    return $ PPGroupPart Text_line [x]

nonEmptyTextLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
nonEmptyTextLine = do
    nonHash <- satisfyT (\(PPToken t val) -> t /= PP_NewLine && ( not (val == "#" && t == Preprocessing_op_or_punc))) <?> "nonEmptyTextLine start"
    x <- many $ noneOfTokenTypes [PP_NewLine]
    matchTokenType PP_NewLine
    return $ PPGroupPart Text_line (nonHash:x)

textLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
textLine = try (emptyTextLine) <|>  try (nonEmptyTextLine)

ppFileParser :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPFile
ppFileParser = PPFile <$> ppGroups

parsePPFile :: [PPToken] -> Either ParseError PPFile
parsePPFile = parse ppFileParser "preproc parse error"
