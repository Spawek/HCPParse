module ParserSpec where

import System.Exit (exitFailure , exitSuccess)
import HCPParse
import Test.Hspec
import Text.ParserCombinators.Parsec

shouldParse :: (Eq a) => (Parser a) -> String -> a -> Bool
shouldParse parser input expectedOutput =
    case (parse parser "err" input) of
        Left _ -> False
        Right output ->
            if output == expectedOutput then True
            else False

shouldNotParse :: (Parser a) -> String -> Bool
shouldNotParse parser input =
    case (parse parser "err" input) of
        Left _ -> True
        Right _ -> False

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

           

main :: IO ()
main = hspec spec


