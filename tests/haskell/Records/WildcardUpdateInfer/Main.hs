{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

movePoint p x y = p { .. }

main = print (x (movePoint (Point 1 2) 5 6), y (movePoint (Point 1 2) 5 6))
