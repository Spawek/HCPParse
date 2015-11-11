import HCPParse
import Lib.Util(concatWith)
import Text.ParserCombinators.Parsec

main :: IO()
main = do
    rawText <- readFile "testIn.cpp"
    putStr "\nRAW TEXT BEGIN\n"
    putStr rawText
    putStr "\nRAW TEXT END\n"
    eitherParsedLines <- return $ parse cppLines "((UNKNOWN))" rawText
    case eitherParsedLines of
        Left err -> print $ "line parse error: " ++ show err
        Right parsedLines -> do
            print parsedLines
            eitherPreparedForPp <- return $ parse joinWhitespaces "((WHITESPACE JOIN))" (concatWith "\n" parsedLines) -- NOTE: line endings are lost here
            case eitherPreparedForPp of
                Left err -> print $ "prepare for preprocessing failed: " ++ show err
                Right preparedForPp -> do
                    putStr "\nPRE PREPROCESSING BEGIN\n"
                    putStr preparedForPp
                    putStr "\nPRE PREPROCESSING END\n"
                    eitherTokens <- return $ parse ppTokenizer "((UNKNOWN PREPROC))" preparedForPp
                    case eitherTokens of
                        Left err -> print $ "pp tokenizer failed: " ++ show err
                        Right tokens -> do
                            putStr "\nPP TOKENS BEGIN\n"
                            print tokens
                            putStr "\nPP TOKENS END\n"
                            putStr "\nREPRINT BEGIN\n"
                            putStr $ concatWith " " $ restringifyTokens tokens
                            putStr "\nREPRINT END\n"