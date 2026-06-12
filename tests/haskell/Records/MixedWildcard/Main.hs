{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int, z :: Int }

makePoint x y = Point { z = 3, .. }

movePoint :: Point -> Int -> Int -> Point
movePoint p x y = p { z = 9, .. }

main = print (((x p1, y p1), z p1), ((x p2, y p2), z p2))
  where
    p1 = makePoint 1 2
    p2 = movePoint (Point 4 5 6) 7 8
