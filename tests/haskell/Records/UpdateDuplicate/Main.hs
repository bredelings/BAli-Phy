{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

p = (Point 1 2) { x = 3, x = 4 }

main = print (x p)
