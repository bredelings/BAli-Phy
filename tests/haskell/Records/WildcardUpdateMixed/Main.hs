{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int, z :: Int }

movePoint p x y = p { z = 9, .. }

main = print (movePoint (Point 1 2 3) 4 5)
