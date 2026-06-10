{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

makePoint x y = Point { x, y }

main = print (x (Point { y = 4, x = 3 }), y (makePoint 5 6))
