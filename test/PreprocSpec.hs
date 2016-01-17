{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module PreprocSpec where

import Preproc
import PPTokenParser
import Tokenizer
import Data.List
import Test.Hspec

readFileAsk :: a -> b -> a
readFileAsk askFoo fileName = askFoo

fakePutStr :: (Monad m) => String -> m()
fakePutStr x = do
    return ()

fakePrint :: (Monad m, Show a) => a -> m()
fakePrint x = do
    return ()

fakeWorld :: (Monad m) => [(String, String)] -> World m
fakeWorld fileList = World {
    worldReadFile = (\arg ->
        case find (\(x, _) -> x == arg) fileList of
            Just (a,b) -> do return b
            Nothing -> do return "DIDNT FIND INCLUDE FILE"),
    worldPutStr = fakePutStr,
    worldPrint = fakePrint
}

restringifyPPGroup :: [PPGroupPart] -> String
restringifyPPGroup groups = intercalate " " $ concatMap (restringifyTokens . groupTokens) groups

main4 :: (Monad m) => m (Either String [PPGroupPart]) -- NOTE: I'm not sure why "(Monad m) => m" is needed
main4 = preprocParser (fakeWorld [("a.cpp", "some content\n")]) "a.cpp"

worldTest :: (Monad m) => World m -> m String
worldTest world = (worldReadFile world) "a.cpp"

preprocTest :: [(String, String)] -> String -> Bool
preprocTest files expectedOutput = 
    case preprocParser (fakeWorld files) "a.cpp" of
            Left _ -> False
            Right t ->  case t of -- TODO: figure out why it returns double either
                Left _ -> False
                Right tokens -> (restringifyPPGroup tokens) == expectedOutput

spec :: Spec
spec = do
    describe "preproc" $ do
        it "can include" $
            preprocTest [("a.cpp", "#include <abc.cpp>\n"), ("abc.cpp", "xxx")] "xxx"
        it "can define" $
            preprocTest [("a.cpp", "#define abc cba\nabc")] "cba"
        it "can define 1 param macro" $
            preprocTest [("a.cpp", "#define abc(x) x + x\n abc(5)")] "5 + 5"
        it "can define 2 param macro" $
            preprocTest [("a.cpp", "#define abc(x, y) x + x * y + y\n abc(5, 7)")] "5 + 5 * 7 + 7"
        it "can take ifdef" $
            preprocTest [("a.cpp", "#define abc\n#ifdef abc\nxxx\n#endif\n")] "xxx"
        it "can not take ifdef" $
            preprocTest [("a.cpp", "#ifdef abc\nabc\n#endif\n")] ""
        it "can take ifndef" $
            preprocTest [("a.cpp", "#ifndef abc\nxxx\n#endif\n")] "xxx"
        it "can not take ifndef" $
            preprocTest [("a.cpp", "#define abc\n#ifndef abc\nxxx\n#endif\n")] ""

main :: IO ()
main = hspec spec
