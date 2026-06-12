{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

makePoint x y = Point { .. }

main = print (x (makePoint 1 2), y (makePoint 1 2))
