import Text.ParserCombinators.Parsec
import System.Environment

data CppLine = CppLine {
    lineContent :: String,
    lineNo :: Int
} deriving (Show)

data PreprocessorDefine = PreprocessorDefine {
    from :: String,
    to :: String
}

-- according to C++14 draft:
--    1) remove slash endlines
--    2) do preprocessing
--        a) preprocessing tokenizer
--        b) 

--applyDefines :: Either ParseError [CppLine] -> Either ParseError [CppLine]
--applyDefines (Left err) = Left err
--applyDefines (Right parsed) = Right $ applyDefines2 [] parsed []

--applyDefines2 :: [PreprocessorDefine] -> [CppLine] -> [CppLine]
--applyDefines2 _ [] x = x
--applyDefines2 defines [nextInput:restInput] out = applyDefines2 newDefinesList xs newOutList
--    where newLine = useDefines defines nextInput
--          newOutList = [newLine:out]
--          newDefinesList = appendDefine nextInput defines

--useDefines ::

--defines without parameters only

--TODO: change it to monad use
makeCppLines :: Either ParseError [String] -> Either ParseError [CppLine]
makeCppLines (Left err) = Left err
makeCppLines (Right parsed) = Right $ makeCppLines2 1 parsed

--BUG: lines with \\\n are not counted
makeCppLines2 :: Int -> [String] -> [CppLine]
makeCppLines2 _ [] = []
makeCppLines2 lineNo (x:xs) = ((CppLine x lineNo):(makeCppLines2 (lineNo + 1) xs))

cppLines = endBy notEOL eolChar
eolChar = oneOf ("\n\r")
notEOL = many notEOLHelp
notEOLHelp = try (char '\\' >> oneOf("\n\r") >> anyChar) <|> (noneOf "\n\r") -- TODO: lines should be counted here - somehow


main = readFile "testIn.cpp" >>= print . makeCppLines . parse cppLines "((UNKNOWN))"
