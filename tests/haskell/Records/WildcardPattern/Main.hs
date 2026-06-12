{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int, z :: Int }

f (Point { .. }) = ((x, y), z)

g (Point { x = a, .. }) = ((a, y), z)

main = print (f (Point 1 2 3), g (Point 4 5 6))
