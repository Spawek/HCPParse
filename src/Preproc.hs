{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Preproc where

import Tokenizer
import PPTokenParser
import Text.Parsec
import Data.List
import Control.Monad (replicateM)

data PreprocResult =
    IncludeRequest {
        alreadyParsed :: [PPGroupPart],
        fileToInclude :: String,
        remainingTokens :: [PPGroupPart],
        parsingState :: ParsingState} |
    CompleteResult {
        resultTokens :: [PPGroupPart],
        parsingState :: ParsingState} |
    ParseErr {errorMessage :: String }
    deriving (Eq, Show)

data ParsingState = ParsingState { --TODO: if it contains just 1 element maybe it's not needed?
    definitions :: [MacroDefinition]
}  deriving (Eq, Show)

appendToken :: PreprocResult -> PPGroupPart -> PreprocResult
appendToken err@(ParseErr _) _ = err
appendToken (CompleteResult x y) newToken = (CompleteResult (newToken:x) y)
appendToken (IncludeRequest a b c d) newToken = (IncludeRequest (newToken:a) b c d)

replaceMacros :: [MacroDefinition] -> [PPToken] -> [PPToken]
replaceMacros [] tokens = tokens
replaceMacros (x:xs) tokens =
    case parse (macroReplacementParser x) "macro replacement parser error" tokens of
        Right tokens -> replaceMacros xs tokens
        -- TODO: is Left possible?

macroReplacementParser :: Stream [PPToken] m PPToken => MacroDefinition -> ParsecT [PPToken] u m [PPToken]
macroReplacementParser x = do
    x <- many1 (try (replac x) <|> justTakeToken)
    return $ concat x

macroReplacementParameter :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m [PPToken]
macroReplacementParameter = do
    x <- many1 (try macroReplacementParameterParens <|> macroReplacementParameterNoParens)
    return $ concat x

macroReplacementParameterParens :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m [PPToken]
macroReplacementParameterParens = do
    lParen <- matchToken Preprocessing_op_or_punc "("
    x <- macroReplacementParameter
    rParen <- matchToken Preprocessing_op_or_punc ")"
    return $ [lParen] ++ x ++ [rParen]

macroReplacementParameterNoParens :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m [PPToken]
macroReplacementParameterNoParens = do
    x <- satisfyT (\(PPToken t val) -> not (t == Preprocessing_op_or_punc && (val == ")" || val == ",")))
    return [x]

nextMacroReplacementParameter :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m [PPToken]
nextMacroReplacementParameter = do
    matchToken Preprocessing_op_or_punc ","
    param <- macroReplacementParameter
    return param

substituteReplacementList :: [PPToken] -> [[PPToken]] -> [PPToken]
substituteReplacementList list params = substituteReplacementList2 0 list params

substituteReplacementList2 :: Int -> [PPToken] -> [[PPToken]] -> [PPToken]
substituteReplacementList2 _ list [] = list
substituteReplacementList2 no list (x:xs) =
    let
        currResult = concatMap (\t -> if t == (PPToken (PP_MacroParameter no) []) then x else [t]) list
    in
        substituteReplacementList2 (no+1) currResult xs        

-- NOTE: multi-line macro invocations are not supported
replac :: Stream [PPToken] m PPToken => MacroDefinition -> ParsecT [PPToken] u m [PPToken]
replac (MacroDefinition name 0 replacementList) = do
    satisfyT (\t -> t == name)
    return replacementList
replac (MacroDefinition name paramsNo replacementList) = do
    satisfyT (\t -> t == name)
    matchToken Preprocessing_op_or_punc "("
    firstParam <- macroReplacementParameter --TODO: macro parameter can have multiple tokens inside? --ERROR!
    nextParams <- (replicateM (paramsNo-1)) nextMacroReplacementParameter
    matchToken Preprocessing_op_or_punc ")"
    return $ substituteReplacementList replacementList (firstParam:nextParams)

justTakeToken :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m [PPToken]
justTakeToken = do
    x <- anyT
    return [x]

performPreproc :: PreprocResult -> [PPGroupPart] -> PreprocResult
performPreproc lastResult@(ParseErr _) _ = lastResult 
performPreproc lastResult@(IncludeRequest _ _ _ _) _ = lastResult
performPreproc lastResult@(CompleteResult _ _) [] = lastResult
performPreproc lastResult@(CompleteResult resultTokens parsingState@(ParsingState definitions)) (x:xs) =
    case x of
        (PPGroupPart (If_section if_group) groupTokens) ->
            case if_group of
                (PPGroupPart (If_group if_groupType subGroups) groupTokens) ->
                    case if_groupType of
                        Ifndef -> takeIfGroupIfDefined (not isTokenDefined)
                        Ifdef -> takeIfGroupIfDefined isTokenDefined
                        where
                            isTokenDefined = elem (head groupTokens) $ map macroName definitions
                            takeIfGroup = performPreproc lastResult (subGroups ++ xs)
                            dontTakeIfGroup = performPreproc lastResult xs
                            takeIfGroupIfDefined isDefined =
                                if isDefined
                                then takeIfGroup
                                else dontTakeIfGroup
        (PPGroupPart (Control_line (Define def@(MacroDefinition _ _ _))) groupTokens) ->
            let
                newParsingState = ParsingState (def:definitions)
                newResult = CompleteResult resultTokens newParsingState
            in
                performPreproc newResult xs
        (PPGroupPart (Control_line Include) groupTokens) ->
            let
                fileToInclude = case head groupTokens of
                    PPToken Header_name text ->
                        text
            in
                IncludeRequest resultTokens fileToInclude xs parsingState
        text@(PPGroupPart Text_line groupTokens) ->
            performPreproc (appendToken lastResult (PPGroupPart Text_line (replaceMacros definitions groupTokens))) xs

tokenizeFile :: (Monad m) => World m -> String -> m (Either String [PPGroupPart])
tokenizeFile world fileName = do
    rawText <- worldReadFile world fileName
    worldPutStr world "\nRAW TEXT BEGIN\n"
    worldPutStr world rawText
    worldPutStr world "\nRAW TEXT END\n"
    case ppTokenize rawText of
        Left err -> do
            worldPrint world $ "tokenizer error: " ++ show err
            return $ (Left (show err))
        Right tokens -> do
            worldPutStr world "\nPP TOKENS BEGIN\n"
            worldPutStr world $ intercalate "\n" $ map show tokens
            worldPutStr world "\nPP TOKENS END\n"
            worldPutStr world "\nREPRINT BEGIN\n"
            worldPutStr world $ intercalate " " $ restringifyTokens tokens
            worldPutStr world "\nREPRINT END\n"
            case parsePPFile tokens of
                Left err -> do
                    worldPrint world $ "preproc parser error: " ++ show err
                    return $ Left (show err)
                Right file@(PPFile parsedTokens) -> do
                    worldPutStr world "\nPARSED TOKENS BEGIN\n"
                    worldPrint world file
                    worldPutStr world "\nPARSED TOKENS END\n"
                    return $ Right parsedTokens

parseTokens :: (Monad m) => World m -> PreprocResult -> [PPGroupPart] -> m PreprocResult
parseTokens world preprocResult tokens = do
    result <- return $ performPreproc preprocResult tokens
    worldPrint world $ "PREPROC DATA BEGIN"
    worldPrint world result
    worldPrint world $ "PREPROC DATA ENDED"
    case result of
        res@(ParseErr _) -> return res
        res@(CompleteResult _ _) -> return res
        res@(IncludeRequest alreadyParsed fileToInclude remainingTokens includedState) -> do
            includeResult <- parseFile world (CompleteResult alreadyParsed includedState) fileToInclude
            worldPrint world $ "INCLUDED DATA BEGIN"
            worldPrint world includeResult
            worldPrint world $ "INCLUDED DATA ENDED"
            parseTokens world includeResult remainingTokens

parseFile :: (Monad m) => World m -> PreprocResult -> String -> m PreprocResult
parseFile world preprocResult fileName = do
    parsedTokens <- tokenizeFile world fileName
    case parsedTokens of
        Left err -> return $ ParseErr err
        Right parsedTokens -> parseTokens world preprocResult parsedTokens

getPostPreprocText :: [PPGroupPart] -> [Char]
getPostPreprocText [] = ""
getPostPreprocText (x:xs) = 
    case x of
        (PPGroupPart Text_line tokens) ->
            foldl (\x -> \y -> x ++ " " ++ y ) "" (map text tokens) ++ "\n" ++ getPostPreprocText xs

data World m = World {
    worldReadFile :: String -> m String,
    worldPutStr :: String -> m(),
    worldPrint :: (Show a) => a -> m()
}

-- based on ephemient answer from:
-- http://stackoverflow.com/questions/984372/how-to-mock-for-testing-in-haskell
realWorld :: World IO
realWorld = World {
    worldReadFile = readFile,
    worldPutStr = putStr,
    worldPrint = print
}

-- they say if the only purpose for IO is logging then Writer monad can be used
-- https://wiki.haskell.org/Avoiding_IO
preprocParser :: (Monad m) => World m -> String -> m (Either String [PPGroupPart])
preprocParser world file = do
    parsed <- parseFile world (CompleteResult [] (ParsingState[] )) file
    case parsed of
        CompleteResult tokens _ -> do
            worldPutStr world $ getPostPreprocText $ reverse tokens
            return $ Right $ reverse tokens
        ParseErr err -> do
            worldPrint world $ "PARSE ERROR: " ++ err
            return $ Left err
        req@(IncludeRequest _ _ _ _) -> do
            worldPrint world $ "PARSER ERROR - INCLUDE RETURNED!!! BEGIN"
            worldPrint world req
            worldPrint world $ "PARSER ERROR - INCLUDE RETURNED!!! END"
            return $ Left "include returned"