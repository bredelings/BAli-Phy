{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

main = print (x (Point { x = 1 }))
