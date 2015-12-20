{-# LANGUAGE FlexibleContexts #-}

module PPTokenParser where 

import Text.Parsec
import HCPParse


type PPFile = [PPGroupPart]

data PPGroupPart = PPGroupPart{
    groupType :: PPGroupPartType,
    groupTokens :: [PPToken]
}  deriving (Eq, Show)

data PPGroupPartType =
    If_section {if_group :: PPGroupPart, endif_line :: PPGroupPart}|
    Control_line |
    Text_line |
    Non_directive |
    If_group {subGroups :: [PPGroupPart]} |
    Endif_line
    deriving (Eq, Show)

-- ppDirective should be on the beggining of the line

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
    y <- endifLine
    return $ PPGroupPart (If_section x y) []

endifLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
endifLine = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "endif"
    matchTokenType PP_NewLine
    return $ PPGroupPart Endif_line [x, y]

-- NOTE: only #ifndef supported for now
ifGroup :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
ifGroup = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "ifndef"
    identifier <- matchTokenType Identifier
    matchTokenType PP_NewLine
    subGroups <- many ppGroup
    return $ PPGroupPart (If_group subGroups) [x, y, identifier]

controlSection :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
controlSection = try (includeLine) <|> try (defineLine)

includeLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
includeLine = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "include"
    z <- many $ noneOfTokenTypes [PP_NewLine] -- NOTE: maybe it should be Header_name instead of anyT, but it is done this way in C++ standard
    matchTokenType PP_NewLine
    return $ PPGroupPart Control_line ([x, y] ++ z)

defineLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
defineLine = do
    x <- matchToken Preprocessing_op_or_punc "#"
    y <- matchToken Identifier "define"
    identifier <- matchTokenType Identifier
    replacement_list <- many $ noneOfTokenTypes [PP_NewLine]
    matchTokenType PP_NewLine
    return $ PPGroupPart Control_line ([x, y, identifier] ++ replacement_list)

textLine :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPGroupPart
textLine = do
    nonHash <- satisfyT (\(PPToken t val) -> t /= PP_NewLine && ( not (val == "#" && t == Preprocessing_op_or_punc))) <?> "textLine start"
    x <- many $ noneOfTokenTypes [PP_NewLine]
    matchTokenType PP_NewLine
    return $ PPGroupPart Text_line (nonHash:x)
