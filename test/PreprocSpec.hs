{-# LANGUAGE FlexibleContexts, RankNTypes, DatatypeContexts #-}

module PreprocSpec where

import Main
import PPTokenParser
import Data.List

-- fakeRead :: a -> a -> IO a
-- fakeRead x y = x

-- fakePrint :: (Show a) => a -> IO ()
-- fakePrint x = ()

readFileAsk :: a -> b -> a
readFileAsk askFoo fileName = askFoo

fakePutStr :: (Monad m) => String -> m()
fakePutStr x = do
    return ()

fakePrint :: (Monad m, Show a) => a -> m()
fakePrint x = do
    return ()

-- fakeWorld :: (Monad m, MonadReader String m) => World m
-- fakeWorld = World {
--     worldReadFile = readFileAsk ask,
--     worldPutStr = fakePutStr,
--     worldPrint = fakePrint
-- }

-- main2 :: ([PPGroupPart], (), [PPGroupPart])
-- main2 = case runRWS (preprocParser fakeWorld "in.cpp") "inside" () of
--     (a, b, out) -> (a, b, out)

-- fakeWorldWithState :: (Monad m, MonadState [(String, String)] m) => World m
-- fakeWorldWithState = World {
--     worldReadFile = (\arg -> do
--         state <- get
--         case find (\(x, _) -> x == arg) state of
--             Just (a,b) -> do return b
--             Nothing -> do return "DIDNT FIND INCLUDE FILE"),
--     worldPutStr = fakePutStr,
--     worldPrint = fakePrint
-- }

-- main3 :: ([PPGroupPart], [(String, String)], [PPGroupPart])
-- main3 = case runRWS (preprocParser fakeWorldWithState "in.cpp") "dsadsa" [("in.cpp", "some content")] of
--     (a, state, out) -> (a, state, out)


fakeWorldWithState2 :: (Monad m) => [(String, String)] -> World m
fakeWorldWithState2 fileList = World {
    worldReadFile = (\arg ->
        case find (\(x, _) -> x == arg) fileList of
            Just (a,b) -> do return b
            Nothing -> do return "DIDNT FIND INCLUDE FILE"),
    worldPutStr = fakePutStr,
    worldPrint = fakePrint
}

main4 :: [[PPGroupPart]]
main4 = preprocParser (fakeWorldWithState2 [("in.cpp", "some content\n")]) "in.cpp" -- IT SHOULD WORK!

worldTest :: (Monad m) => World m -> m String
worldTest world = (worldReadFile world) "in.cpp"

-- arg: worldTest (fakeWorldWithState2 [("in.cpp", "some content")])

-- data (Monad m) => ServiceImplementation m = ServiceImplementation
--   {
--       serviceGetLine :: m String,
--       servicePutLine :: String -> m ()
--   }

-- serviceHelloBase :: (Monad m) => ServiceImplementation m -> m ()
-- serviceHelloBase impl = do
--     name <- serviceGetLine impl
--     servicePutLine impl $ "Hello, " ++ name

-- realImpl2 :: ServiceImplementation IO
-- realImpl2 = ServiceImplementation
--     {
--         serviceGetLine = getLine,
--         servicePutLine = putStrLn
--     }

-- mockImpl :: (Monad m, MonadReader String m, MonadWriter String m) =>
--     ServiceImplementation m
-- mockImpl = ServiceImplementation
--     {
--         serviceGetLine = ask,
--         servicePutLine = tell
--     }

-- main = serviceHello realImpl2
-- test = case runRWS (serviceHello mockImpl) "Dave" () of
--     (_, _, "Hello, Dave") -> True; _ -> False

-- main = serviceHelloBase realImpl2
-- test = case runRWS (serviceHelloBase mockImpl) "Dave" () of
--     (_, _, "Hello, Dave") -> True; _ -> False