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

tokenizeFile :: String -> IO (Either String [PPGroupPart])
tokenizeFile fileName = do
    rawText <- readFile fileName
    putStr "\nRAW TEXT BEGIN\n"
    putStr rawText
    putStr "\nRAW TEXT END\n"
    case ppTokenize rawText of
        Left err -> do
            print $ "tokenizer error: " ++ show err
            return $ (Left (show err))
        Right tokens -> do
            putStr "\nPP TOKENS BEGIN\n"
            putStr $ concatWith "\n" $ map show tokens
            putStr "\nPP TOKENS END\n"
            putStr "\nREPRINT BEGIN\n"
            putStr $ concatWith " " $ restringifyTokens tokens
            putStr "\nREPRINT END\n"
            case parsePPFile tokens of
                Left err -> do
                    print $ "preproc parser error: " ++ show err
                    return $ Left (show err)
                Right file@(PPFile parsedTokens) -> do
                    putStr "\nPARSED TOKENS BEGIN\n"
                    print file
                    putStr "\nPARSED TOKENS END\n"
                    return $ Right parsedTokens

parseTokens :: PreprocResult -> [PPGroupPart] -> IO PreprocResult
parseTokens preprocResult tokens = do
    result <- return $ performPreproc preprocResult tokens
    print $ "PREPROC DATA BEGIN"
    print result
    print $ "PREPROC DATA ENDED"
    case result of
        res@(ParseErr _) -> return res
        res@(CompleteResult _ _) -> return res
        res@(IncludeRequest alreadyParsed fileToInclude remainingTokens includedState) -> do
            includeResult <- parseFile (CompleteResult alreadyParsed includedState) fileToInclude
            print $ "INCLUDED DATA BEGIN"
            print includeResult
            print $ "INCLUDED DATA ENDED"
            parseTokens includeResult remainingTokens

parseFile :: PreprocResult -> String -> IO PreprocResult
parseFile preprocResult fileName = do
    parsedTokens <- tokenizeFile fileName
    case parsedTokens of
        Left err -> return $ ParseErr err
        Right parsedTokens -> parseTokens preprocResult parsedTokens

getPostPreprocText :: [PPGroupPart] -> [Char]
getPostPreprocText [] = ""
getPostPreprocText (x:xs) = 
    case x of
        (PPGroupPart Text_line tokens) ->
            foldl (\x -> \y -> x ++ " " ++ y ) "" (map text tokens) ++ "\n" ++ getPostPreprocText xs

main :: IO()
main = do
    parsed <- parseFile (CompleteResult [] (ParsingState[] )) "testSmallIn2.cpp"
    case parsed of
        CompleteResult tokens _ -> do
            putStr $ getPostPreprocText $ reverse tokens
        ParseErr err -> do
            print $ "PARSE ERROR: " ++ err
        req@(IncludeRequest _ _ _ _) -> do
            print $ "PARSER ERROR - INCLUDE RETURNED!!! BEGIN"
            print req
            print $ "PARSER ERROR - INCLUDE RETURNED!!! END"
    return ()