{-# LANGUAGE RankNTypes, FlexibleContexts #-}

import Tokenizer
import PPTokenParser
import Text.Parsec
import Lib.Util (concatWith)
import Control.Monad (replicateM)

-- NOTE: maybe alreadyparsed should be PPTokens?
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

-- replaceMacros :: ParsingState -> [PPToken] -> [PPToken]
-- replaceMacros (ParsingState definitions) tokens = concatMap (replaceMacros2 definitions) tokens

replaceMacros :: ParsingState -> [PPToken] -> [PPToken] -- with macro functions
replaceMacros (ParsingState definitions) tokens =  replaceMacros2 definitions tokens

replaceMacros2 :: [MacroDefinition] -> [PPToken] -> [PPToken] -- TODO: it's basicly the same as 1 so can be merged
replaceMacros2 [] tokens = tokens
replaceMacros2 (x:xs) tokens =
    case parse (macroReplacementParser x) "macro replacement parser error" tokens of
        Right tokens -> replaceMacros2 xs tokens
        -- TODO: is Left possible?

macroReplacementParser :: Stream [PPToken] m PPToken => MacroDefinition -> ParsecT [PPToken] u m [PPToken]
macroReplacementParser x = do
    x <- many (try (replac x) <|> justTakeToken) --NOTE: to 2 chyba nie przejdzie, bo jest w many a moe przyjac wszywsko - wiec caly parser przyjmuje cokolwiek (chociaz w sumie on ma tak dzialac)
    return $ concat x

macroReplacementParameter :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPToken
macroReplacementParameter = satisfyT (\(PPToken t val) -> not (t == Preprocessing_op_or_punc && (val == ")" || val == ","))) -- TODO: parenthesis should be allowed here

nextMacroReplacementParameter :: Stream [PPToken] m PPToken => ParsecT [PPToken] u m PPToken
nextMacroReplacementParameter = do
    matchToken Preprocessing_op_or_punc ","
    param <- macroReplacementParameter
    return param

substituteReplacementList :: [PPToken] -> [PPToken] -> [PPToken]
substituteReplacementList list params = substituteReplacementList2 0 list params

substituteReplacementList2 :: Int -> [PPToken] -> [PPToken] -> [PPToken]
substituteReplacementList2 _ list [] = list
substituteReplacementList2 no list (x:xs) =
    let
        currResult = map (\t -> if t == (PPToken (PP_MacroParameter no) []) then x else t) list -- TODO: when multiple tokens - just change to concatMap and t to [t]
    in
        substituteReplacementList2 (no+1) currResult xs        

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
performPreproc lastResult@(CompleteResult resultTokens parsingState) (x:xs) =
    case x of
        (PPGroupPart (If_section if_group) groupTokens) ->
            case if_group of
                (PPGroupPart (If_group if_groupType subGroups) groupTokens) ->
                    case if_groupType of
                        Ifndef -> takeIfGroupIfDefined (not isTokenDefined)
                        Ifdef -> takeIfGroupIfDefined isTokenDefined
                        where
                            isTokenDefined = elem (head groupTokens) $ map macroName (definitions parsingState)
                            takeIfGroup = performPreproc lastResult (subGroups ++ xs)
                            dontTakeIfGroup = performPreproc lastResult xs
                            takeIfGroupIfDefined isDefined =
                                if isDefined
                                then takeIfGroup
                                else dontTakeIfGroup
        (PPGroupPart (Control_line (Define def@(MacroDefinition _ _ _))) groupTokens) ->
            let
                newParsingState = ParsingState (def:(definitions parsingState))
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
            performPreproc (appendToken lastResult (PPGroupPart Text_line (replaceMacros parsingState groupTokens))) xs

tokenizeFile :: World IO -> String -> IO (Either String [PPGroupPart]) --TODO: this file doesn't need IO at all - it can get a string on input
tokenizeFile impl fileName = do
    rawText <- worldReadFile impl fileName
    worldPutStr impl "\nRAW TEXT BEGIN\n"
    worldPutStr impl rawText
    worldPutStr impl "\nRAW TEXT END\n"
    case ppTokenize rawText of
        Left err -> do
            worldPrint impl $ "tokenizer error: " ++ show err
            return $ (Left (show err))
        Right tokens -> do
            worldPutStr impl "\nPP TOKENS BEGIN\n"
            worldPutStr impl $ concatWith "\n" $ map show tokens
            worldPutStr impl "\nPP TOKENS END\n"
            worldPutStr impl "\nREPRINT BEGIN\n"
            worldPutStr impl $ concatWith " " $ restringifyTokens tokens
            worldPutStr impl "\nREPRINT END\n"
            case parsePPFile tokens of
                Left err -> do
                    worldPrint impl $ "preproc parser error: " ++ show err
                    return $ Left (show err)
                Right file@(PPFile parsedTokens) -> do
                    worldPutStr impl "\nPARSED TOKENS BEGIN\n"
                    worldPrint impl file
                    worldPutStr impl "\nPARSED TOKENS END\n"
                    return $ Right parsedTokens

parseTokens :: World IO -> PreprocResult -> [PPGroupPart] -> IO PreprocResult
parseTokens impl preprocResult tokens = do
    result <- return $ performPreproc preprocResult tokens
    worldPrint impl $ "PREPROC DATA BEGIN"
    worldPrint impl result
    worldPrint impl $ "PREPROC DATA ENDED"
    case result of
        res@(ParseErr _) -> return res
        res@(CompleteResult _ _) -> return res
        res@(IncludeRequest alreadyParsed fileToInclude remainingTokens includedState) -> do
            includeResult <- parseFile impl (CompleteResult alreadyParsed includedState) fileToInclude
            worldPrint impl $ "INCLUDED DATA BEGIN"
            worldPrint impl includeResult
            worldPrint impl $ "INCLUDED DATA ENDED"
            parseTokens impl includeResult remainingTokens

parseFile :: World IO -> PreprocResult -> String -> IO PreprocResult
parseFile impl preprocResult fileName = do
    parsedTokens <- tokenizeFile impl fileName
    case parsedTokens of
        Left err -> return $ ParseErr err
        Right parsedTokens -> parseTokens impl preprocResult parsedTokens

getPostPreprocText :: [PPGroupPart] -> [Char]
getPostPreprocText [] = ""
getPostPreprocText (x:xs) = 
    case x of
        (PPGroupPart Text_line tokens) ->
            foldl (\x -> \y -> x ++ " " ++ y ) "" (map text tokens) ++ "\n" ++ getPostPreprocText xs

data World m  = World {
    worldReadFile :: String -> m String,
    worldPutStr :: String -> m(),
    worldPrint :: (Show a) => a -> m()
}

realImpl :: World IO
realImpl = World {
    worldReadFile = readFile,
    worldPutStr = putStr,
    worldPrint = print
}

main2 :: World IO -> IO()
main2 impl = do
    parsed <- parseFile impl (CompleteResult [] (ParsingState[] )) "testSmallIn3.cpp"
    case parsed of
        CompleteResult tokens _ -> do
            worldPutStr impl $ getPostPreprocText $ reverse tokens
        ParseErr err -> do
            worldPrint impl $ "PARSE ERROR: " ++ err
        req@(IncludeRequest _ _ _ _) -> do
            worldPrint impl $ "PARSER ERROR - INCLUDE RETURNED!!! BEGIN"
            worldPrint impl req
            worldPrint impl $ "PARSER ERROR - INCLUDE RETURNED!!! END"
    return ()

main :: IO()
main = do
    main2 realImpl