{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module PreprocSpec where

import Preproc
import PPTokenParser
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

fakeWorldWithState2 :: (Monad m) => [(String, String)] -> World m
fakeWorldWithState2 fileList = World {
    worldReadFile = (\arg ->
        case find (\(x, _) -> x == arg) fileList of
            Just (a,b) -> do return b
            Nothing -> do return "DIDNT FIND INCLUDE FILE"),
    worldPutStr = fakePutStr,
    worldPrint = fakePrint
}

main4 :: (Monad m) => m (Either String [PPGroupPart]) -- NOTE: I'm not sure why "(Monad m) => m" is needed
main4 = preprocParser (fakeWorldWithState2 [("in.cpp", "some content\n")]) "in.cpp"

worldTest :: (Monad m) => World m -> m String
worldTest world = (worldReadFile world) "in.cpp"

spec :: Spec
spec = do
    describe "aaa" $ do
        it "dsa" $ True