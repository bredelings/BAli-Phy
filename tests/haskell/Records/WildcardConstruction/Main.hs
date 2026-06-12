{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

makePoint x y = Point { .. }

movePoint :: Point -> Int -> Int -> Point
movePoint p x y = p { .. }

main = print (x (makePoint 1 2), y (movePoint (Point 3 4) 5 6))
