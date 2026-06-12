{-# LANGUAGE NoImplicitPrelude, NoFieldSelectors #-}

module Main where

import Compiler.Num
import System.IO (print)

data Point = Point { x :: Int, y :: Int }

setY p b = p { y = b }

readPoint (Point { x = a, y = b }) = (a, b)

main = print (readPoint (setY (Point { x = 1, y = 2 }) 7))
