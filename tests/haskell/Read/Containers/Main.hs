{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Bool
import Data.Eq
import System.IO
import Text.Read

checkUnit :: () -> Bool
checkUnit () = True

checkTuple4 :: (Int, Int, Int, Int) -> Bool
checkTuple4 (1, 2, 3, 4) = True
checkTuple4 _ = False

ok :: Bool
ok =
    checkUnit (read "()") &&
    read "(1,2)" == (1 :: Int, 2 :: Int) &&
    read "(1,2,3)" == (1 :: Int, 2 :: Int, 3 :: Int) &&
    checkTuple4 (read "(1,2,3,4)") &&
    read "[1, 2, 3]" == ([1, 2, 3] :: [Int]) &&
    read "'a'" == 'a' &&
    read "'\\n'" == '\n' &&
    read "\"ab\\n\\\"\"" == "ab\n\"" &&
    read "[\"a\", \"b\"]" == ["a", "b"]

main = putStrLn (if ok then "ok" else "bad")
