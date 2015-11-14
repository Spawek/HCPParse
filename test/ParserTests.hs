module ParserTests where

import System.Exit (exitFailure , exitSuccess)
import HCPParse
import Test.Hspec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

-- to run test:
--   "hspec spec"

-- main = do
--     putStrLn "This test always pass!"
--     exitSuccess
--     -- exitFailure
--     -- putStrLn "This test always fails!"

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

        -- `shouldBe` (Left (ParseError "err"))




