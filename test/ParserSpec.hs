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

shoudNotParse :: (Parser a) -> String -> Bool
shoudNotParse parser input =
    case (parse parser "err" input) of
        Left _ -> True
        Right _ -> False

spec :: Spec
spec = do
    describe "cppNondigit" $ do
        it "should parse 'a'" $
            shouldParse cppNondigit "a" 'a'
        it "shouldn't parse '4'" $
            shoudNotParse cppNondigit "4"

    describe "cppDigit" $ do
        it "should parse '6'" $
            shouldParse cppDigit "6" '6'
        it "shouldn't parse 'f'" $
            shoudNotParse cppDigit "f"

    describe "preprocessing_op_or_punc" $ do
        it "should parse '<='" $
            shouldParse preprocessing_op_or_punc "<=" (PPToken Preprocessing_op_or_punc "<=")
        it  "shouldn't parse 'xxx'" $
            shoudNotParse preprocessing_op_or_punc "xxx"
           

main :: IO ()
main = hspec spec


