module TokenizerSpec where

import Tokenizer
import ParserTestBase
import Test.Hspec
import Text.Parsec
import System.Exit (exitFailure , exitSuccess)
 
ppTokenizeShouldParse :: String -> [PPToken] -> Bool
ppTokenizeShouldParse input expectedOutput =
    case ppTokenize input of
        Right x -> x == expectedOutput
        Left _ -> False

spec :: Spec
spec = do
    describe "cppNondigit" $ do
        it "should parse 'a'" $
            shouldParse cppNondigit "a" 'a'
        it "shouldn't parse '4'" $
            shouldNotParse cppNondigit "4"
    describe "cppDigit" $ do
        it "should parse '6'" $
            shouldParse cppDigit "6" '6'
        it "shouldn't parse 'f'" $
            shouldNotParse cppDigit "f"

    describe "preprocessing_op_or_punc" $ do
        it "should parse '<='" $
            shouldParse preprocessing_op_or_punc "<="
                (PPToken Preprocessing_op_or_punc "<=")
        it  "shouldn't parse 'xxx'" $
            shouldNotParse preprocessing_op_or_punc "xxx"

    describe "header_name" $ do
        it "should parse '<iostream>'" $
            shouldParse header_name "<iostream>"
                (PPToken Header_name "<iostream>")
        it "should parse '\"foo.h\"'" $
            shouldParse header_name "\"foo.h\"" 
                (PPToken Header_name "\"foo.h\"")
        it "shouldn't parse 'TestName'" $
            shouldNotParse header_name "TestName"

    describe "character_literal" $ do
        it "should parse 'abcdef'" $
            shouldParse character_literal "'abcdef'" 
                (PPToken Character_literal "'abcdef'")
        it "should parse U'abcdef'" $
            shouldParse character_literal "U'abcdef'" 
                (PPToken Character_literal "U'abcdef'")
        it "should parse u8'abcdef'" $
            shouldParse character_literal "u8'abcdef'" 
                (PPToken Character_literal "u8'abcdef'")
        it "shouldn't parse xxx'" $
            shouldNotParse character_literal "xxx"
        it "shouldn't parse u5'xxx'" $
            shouldNotParse character_literal "u5'xxx'"

    describe "string_literal" $ do
        it "should parse \"abc\"" $
            shouldParse string_literal "\"abc\""
                (PPToken String_literal "\"abc\"")
        it "should parse U\"abc\"" $
            shouldParse string_literal "U\"abc\""
                (PPToken String_literal "U\"abc\"")
        it "should parse R\"asd(123\\n)asd\"" $
            shouldParse string_literal "R\"asd(123\\n)asd\""  
                (PPToken String_literal "R\"asd(123\\n)asd\"")
        it "should't parse R\"xxx(123)yyy\"" $
            shouldNotParse string_literal "R\"xxx(123)yyy\""
        it "shouldn't parse xxx" $
            shouldNotParse string_literal "xxx"

    describe "joinWhitespaces" $ do
        it "should parse 'abc'" $
            shouldParse joinWhitespaces "abc" "abc"
        it "should parse 'abc '" $
            shouldParse joinWhitespaces "abc " "abc "
        it "should parse ''" $
            shouldParse joinWhitespaces "" ""
        it "should parse ' '" $
            shouldParse joinWhitespaces " " " "
        it "should parse '       '" $
            shouldParse joinWhitespaces "       " " "
        it "should parse '  \\n\\n\\n  x  \\n\\n\\n     '" $
            shouldParse joinWhitespaces "  \n\n\n  x  \n\n\n     " "\nx\n"

    describe "ppTokenize" $ do
        it "should parse single line file" $
            ppTokenizeShouldParse "#" [PPToken Preprocessing_op_or_punc "#"]
        it "should parse multi line file" $
            ppTokenizeShouldParse "#\n"
                [PPToken Preprocessing_op_or_punc "#", PPToken PP_NewLine "\n"]
        it "should parse text with whitespace at the end" $
            ppTokenizeShouldParse "# \n"
                [PPToken Preprocessing_op_or_punc "#", PPToken PP_NewLine "\n"]
        it "should parse empty text" $
            ppTokenizeShouldParse "" []
        it "should parse whitespace" $
            ppTokenizeShouldParse " " []

main :: IO ()
main = hspec spec
