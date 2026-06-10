{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

p = Point { x = 1, z = 2 }

main = print (x p)
