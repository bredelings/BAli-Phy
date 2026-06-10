{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

bad (Point { x = a, x = b }) = a + b

main = print (bad (Point 1 2))
