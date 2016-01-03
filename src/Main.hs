{-# LANGUAGE DatatypeContexts, RankNTypes, ConstraintKinds #-}
-- TODO: don't know what DatatypeContexts and ConstraintKinds really is (and DatatypeContexts is reprecated)

import Tokenizer
import PPTokenParser
import Text.ParserCombinators.Parsec
import Lib.Util (concatWith)

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

data ParsingState = ParsingState{
    definitions :: [(PPToken, [PPToken])]
}  deriving (Eq, Show)

appendToken :: PreprocResult -> PPGroupPart -> PreprocResult
appendToken err@(ParseErr _) _ = err
appendToken (CompleteResult x y) newToken = (CompleteResult (newToken:x) y)
appendToken (IncludeRequest a b c d) newToken = (IncludeRequest (newToken:a) b c d)

replaceMacros :: ParsingState -> [PPToken] -> [PPToken]
replaceMacros (ParsingState definitions) tokens = concatMap (replaceMacros2 definitions) tokens

replaceMacros2 :: [(PPToken, [PPToken])] -> PPToken -> [PPToken]
replaceMacros2 [] token = [token]
replaceMacros2 (x:xs) token =
    let
        curr = if def == token
               then repl
               else [token]
    in
        concatMap (replaceMacros2 xs) curr
    where
        def = fst x
        repl = snd x

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
                            isTokenDefined = elem (head groupTokens) $ map fst (definitions parsingState)
                            takeIfGroup = performPreproc lastResult (subGroups ++ xs)
                            dontTakeIfGroup = performPreproc lastResult xs
                            takeIfGroupIfDefined isDefined =
                                if isDefined
                                then takeIfGroup
                                else dontTakeIfGroup
        (PPGroupPart (Control_line Define) groupTokens) ->
            let
                newDefinition = (head groupTokens, tail groupTokens)
                newParsingState = ParsingState (newDefinition:(definitions parsingState))
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
            worldPutStr impl $ show ("tokenizer error: " ++ show err)
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
                    worldPutStr impl $ show ("preproc parser error: " ++ show err)
                    return $ Left (show err)
                Right file@(PPFile parsedTokens) -> do
                    worldPutStr impl "\nPARSED TOKENS BEGIN\n"
                    worldPutStr impl $ show (file)
                    worldPutStr impl "\nPARSED TOKENS END\n"
                    return $ Right parsedTokens

parseTokens :: World IO -> PreprocResult -> [PPGroupPart] -> IO PreprocResult
parseTokens impl preprocResult tokens = do
    result <- return $ performPreproc preprocResult tokens
    worldPutStr impl $ show ("PREPROC DATA BEGIN")
    worldPutStr impl $ show (result)
    worldPutStr impl $ show ("PREPROC DATA ENDED")
    case result of
        res@(ParseErr _) -> return res
        res@(CompleteResult _ _) -> return res
        res@(IncludeRequest alreadyParsed fileToInclude remainingTokens includedState) -> do
            includeResult <- parseFile impl (CompleteResult alreadyParsed includedState) fileToInclude
            worldPutStr impl $ show ("INCLUDED DATA BEGIN")
            worldPutStr impl $ show (includeResult)
            worldPutStr impl $ show ("INCLUDED DATA ENDED")
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
    worldPutStr :: String -> m()
    -- worldPrint :: show a => a -> m()
    -- worldPrint :: String -> m()
}

realImpl :: World IO
realImpl = World {
    worldReadFile = readFile,
    worldPutStr = putStr
    -- worldPrint = print
}

main2 :: World IO -> IO()
main2 impl = do
    parsed <- parseFile impl (CompleteResult [] (ParsingState[] )) "testSmallIn2.cpp"
    case parsed of
        CompleteResult tokens _ -> do
            worldPutStr impl $ getPostPreprocText $ reverse tokens
        ParseErr err -> do
            worldPutStr impl $ show ("PARSE ERROR: " ++ err)
        req@(IncludeRequest _ _ _ _) -> do
            worldPutStr impl $ show ("PARSER ERROR - INCLUDE RETURNED!!! BEGIN")
            worldPutStr impl $ show (req)
            worldPutStr impl $ show ("PARSER ERROR - INCLUDE RETURNED!!! END")
    return ()

main :: IO()
main = do
    main2 realImpl