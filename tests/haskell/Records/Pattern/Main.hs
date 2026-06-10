{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

sumPoint (Point { y = b, x = a }) = a + b

getX (Point { x }) = x

main = print (sumPoint (Point 3 4), getX (Point 5 6))
