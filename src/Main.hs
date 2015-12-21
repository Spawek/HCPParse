import Tokenizer
import PPTokenParser
import Text.ParserCombinators.Parsec
import Lib.Util (concatWith)

main :: IO()
main = do
    rawText <- readFile "testIn.cpp"
    putStr "\nRAW TEXT BEGIN\n"
    putStr rawText
    putStr "\nRAW TEXT END\n"
    case ppTokenize rawText of
        Left err -> print $ "tokenizer error: " ++ show err
        Right tokens -> do
            putStr "\nPP TOKENS BEGIN\n"
            putStr $ concatWith "\n" $ map show tokens
            putStr "\nPP TOKENS END\n"
            putStr "\nREPRINT BEGIN\n"
            putStr $ concatWith " " $ restringifyTokens tokens
            putStr "\nREPRINT END\n"
            case parsePPFile tokens of
                Left err -> print $ "preproc parser error: " ++ show err
                Right parsedTokens -> do
                    putStr "\nPARSED TOKENS BEGIN\n"
                    print $ parsedTokens
                    putStr "\nPARSED TOKENS END\n"