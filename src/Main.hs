import Tokenizer
import PPTokenParser
import Text.ParserCombinators.Parsec
import Lib.Util (concatWith)

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

joinResults :: PreprocResult -> PreprocResult -> PPGroupPart -> PreprocResult
joinResults err@(ParseErr _) _ _ = err
joinResults _ err@(ParseErr _) _ = err
joinResults (CompleteResult a _) (CompleteResult x y) newTokens = (CompleteResult (a ++ x ++ [newTokens]) y)
joinResults (CompleteResult x _) (IncludeRequest a b c d) newTokens = (IncludeRequest (a ++ x ++ [newTokens]) b c d)

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
                        Ifndef -> if elem (head groupTokens) $ map fst (definitions parsingState)
                            then performPreproc lastResult xs 
                            else
                                let
                                    subResult = performPreproc lastResult subGroups
                                in
                                    performPreproc subResult xs
        text@(PPGroupPart Text_line groupTokens) ->
            let
                nextResult = performPreproc lastResult xs
            in
                joinResults lastResult nextResult text


parseFile :: PreprocResult -> String -> IO PreprocResult
parseFile preprocResult fileName = do
    rawText <- readFile fileName
    putStr "\nRAW TEXT BEGIN\n"
    putStr rawText
    putStr "\nRAW TEXT END\n"
    case ppTokenize rawText of
        Left err -> do
            print $ "tokenizer error: " ++ show err
            return $ ParseErr $ show err
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
                    return $ ParseErr $ show err
                Right (PPFile parsedTokens) -> do
                    putStr "\nPARSED TOKENS BEGIN\n"
                    print parsedTokens
                    putStr "\nPARSED TOKENS END\n"
                    case performPreproc preprocResult parsedTokens of
                        res@(ParseErr _) -> do
                            print $ "SOME ERR BEGIN" 
                            print res
                            print $ "SOME ERR ENDED"
                            return res
                        res@(CompleteResult _ _) -> do
                            print $ "SOME RESULT BEGIN" 
                            print res
                            print $ "SOME RESULT ENDED"
                            return res
                        res@(IncludeRequest alreadyParsed fileToInclude remainingTokens includedState) -> do 
                            print $ "SOME INCLUDE BEGIN" 
                            print res
                            print $ "SOME INCLUDE ENDED"
                            includeResult <- parseFile res fileToInclude -- NOTE: recursive call!
                            return $ performPreproc includeResult remainingTokens -- NOTE: recursive call!
                            -- case includeResult of
                            --     (CompleteResult justParsed justState) -> do  
                            --         remainingResult <- return $ performPreproc justState remainingTokens
                            --         case remainingResult of
                            --             res@(ParseErr _) -> do
                            --                 return res
                            --             (CompleteResult justJustParsed justJustState) -> do
                            --                 return $ CompleteResult (alreadyParsed ++ justParsed ++ justJustParsed) justJustState
                            --             (IncludeRequest newAlreadyParsed newFileToInclude newRemainingTokens justJustState) -> do
                            --                 return $ IncludeRequest (alreadyParsed ++ newAlreadyParsed) newFileToInclude newRemainingTokens justJustState
                            --     (IncludeRequest newAlreadyParsed newFileToInclude newRemainingTokens newState) -> do
                            --         return $ IncludeRequest (alreadyParsed ++ newAlreadyParsed) newFileToInclude newRemainingTokens newState
                            --     err@(ParseErr _) -> do
                            --         return err

main :: IO()
main = do
    parsed <- parseFile (CompleteResult [] (ParsingState[] )) "testSmallIn.cpp"
    print $ parsed
    return ()