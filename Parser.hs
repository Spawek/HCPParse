import Text.ParserCombinators.Parsec
import System.Environment

data CppLine = CppLine {
    lineContent :: String,
    lineNo :: Int
} deriving (Show)


--TODO: change it to monad use
makeCppLines :: Either ParseError [String] -> Either ParseError [CppLine]
makeCppLines (Left err) = Left err
makeCppLines (Right parsed) = Right $ makeCppLines2 1 parsed

makeCppLines2 :: Int -> [String] -> [CppLine]
makeCppLines2 _ [] = []
makeCppLines2 lineNo (x:xs) = ((CppLine x lineNo):(makeCppLines2 (lineNo + (lineIncrement x)) xs))
    where lineIncrement line = 1 + length (filter (\x -> ((x == '\n') || (x == '\r'))) line) -- kurwa te linie juz sa wyciete ...

cppLines = endBy notEOL eolChar
eolChar = oneOf ("\n\r")
notEOL = many notEOLHelp
notEOLHelp = try (char '\\' >> oneOf("\n\r") >> anyChar) <|> (noneOf "\n\r") -- TODO: lines should be counted here - somehow

main :: IO()
main = readFile "testIn.cpp" >>= print . makeCppLines . parse cppLines "((UNKNOWN))"
