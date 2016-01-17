module Main where

import Preproc

main :: IO()
main = do
    preprocParser realImpl "testSmallIn3.cpp"
    return ()