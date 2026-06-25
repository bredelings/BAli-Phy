{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

(--+) :: Int -> Int -> Int
x --+ y = x * (10 :: Int) + y

commented :: Int
commented = (4 :: Int) -- this text must stay a comment

main = print (commented --+ (2 :: Int))
