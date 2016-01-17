module Main where

import Preproc

main :: IO()
main = do
    preprocParser realWorld "testSmallIn3.cpp"
    return ()