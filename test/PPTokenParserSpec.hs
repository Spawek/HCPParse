{-# LANGUAGE FlexibleContexts #-}

module PPTokenParserSpec where

import Tokenizer
import PPTokenParser
import ParserTestBase
import Test.Hspec
import Text.Parsec
import Data.Functor.Identity

shouldCounsumeWholeInput :: (Eq a, Stream s Identity t, Show t) => Parsec s () a -> s -> Bool
shouldCounsumeWholeInput parser input = 
    case (parse (parser >> eof) "err" input) of
        Right _ -> True
        Left _ -> False

quickParse :: String -> [PPToken]
quickParse x =
    case ppTokenize x of
        Right result -> result
        Left err -> error $ "wrong quickParse for: '" ++ x ++ "'"

spec :: Spec
spec = do
    describe "matchTokenType" $ do
        it "should match same token type" $
            shouldParse (matchTokenType Identifier) [PPToken Identifier "abc"]
                (PPToken Identifier "abc")
        it "shouldn't parse different token type" $
            shouldNotParse (matchTokenType Identifier) [PPToken Header_name "abc"] 
        it "shouldn't parse different empty list" $
            shouldNotParse (matchTokenType Identifier) []

    describe "matchToken" $ do
        it "should match same token" $
            shouldParse (matchToken Identifier "abc") [PPToken Identifier "abc"]
                (PPToken Identifier "abc")
        it "shouldn't match different type" $
            shouldNotParse (matchToken Identifier "abc") [PPToken Header_name "abc"]
        it "shouldn't match different text" $
            shouldNotParse (matchToken Identifier "abc") [PPToken Identifier "xxx"]

    describe "ifGroup" $ do
        it "should parse '#ifndef ABC\\n'" $
            shouldParse ifGroup (quickParse "#ifndef ABC\n")
                (PPGroupPart (If_group Ifndef []) [PPToken Identifier "ABC"])

    describe "ifSection" $ do
        it "should parse '#ifndef ABC\\n #endif\\n'" $
            shouldCounsumeWholeInput ifSection (quickParse "#ifndef ABC\n #endif\n")
        it "should parse '#ifndef ABC\\n#ifndef XXX\\n#endif\\n#endif\\n'" $
            shouldCounsumeWholeInput ifSection (quickParse "#ifndef ABC\n#ifndef XXX\n#endif\n#endif\n")

    describe "controlSection" $ do
        it "should parse '#include <iostream>\\n'" $
            shouldCounsumeWholeInput controlSection (quickParse "#include <iostream>\n")
        it "should parse '#define abc xxx\\n'" $
            shouldCounsumeWholeInput controlSection (quickParse "#define abc xxx\n")

    describe "parsePPFile" $ do
    	it "should parse empty line" $
    		case parsePPFile (quickParse "\n") of
    			Left _ -> False
    			Right _ -> True

main :: IO ()
main = hspec spec
