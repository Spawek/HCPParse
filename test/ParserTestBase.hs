{-# LANGUAGE FlexibleContexts #-}

module ParserTestBase where

import Text.Parsec
import Data.Functor.Identity

shouldParse :: (Eq a, Stream s Identity t) => Parsec s () a -> s -> a -> Bool
shouldParse parser input expectedOutput =
    case (parse parser "err" input) of
        Left _ -> False
        Right output ->
            if output == expectedOutput then True
            else False

shouldNotParse :: (Eq a, Stream s Identity t) => Parsec s () a -> s -> Bool
shouldNotParse parser input =
    case (parse parser "err" input) of
        Left _ -> True
        Right _ -> False
