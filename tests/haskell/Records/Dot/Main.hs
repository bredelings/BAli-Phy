{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

main = print ((Point 3 4).x, (.y) (Point 3 4))
