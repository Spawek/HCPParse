import HCPParse
import Lib.Util(concatWith)
import Text.ParserCombinators.Parsec

mainPpTokenize rawText = do
    parsedLines <- parse cppLines "((UNKNOWN CPPLINES))" rawText
    preparedForPp <- parse joinWhitespaces "((WHITESPACE JOIN))" (concatWith "\n" parsedLines) -- NOTE: line endings are lost here
    tokens <- parse ppTokenizer "((UNKNOWN PREPROC))" preparedForPp
    return tokens

main :: IO()
main = do
    rawText <- readFile "testIn.cpp"
    putStr "\nRAW TEXT BEGIN\n"
    putStr rawText
    putStr "\nRAW TEXT END\n"
    case mainPpTokenize rawText of
        Left err -> print $ "tokenizer error: " ++ show err
        Right tokens -> do
            putStr "\nPP TOKENS BEGIN\n"
            putStr $ concatWith "\n" $ map show tokens
            putStr "\nPP TOKENS END\n"
            putStr "\nREPRINT BEGIN\n"
            putStr $ concatWith " " $ restringifyTokens tokens
            putStr "\nREPRINT END\n"
