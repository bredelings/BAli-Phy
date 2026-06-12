{-# LANGUAGE NoImplicitPrelude, NoFieldSelectors #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

makePoint a b = Point { x = a, y = b }

readPoint (Point { x = a, y = b }) = (a, b)

main = print (readPoint (makePoint 1 2))
