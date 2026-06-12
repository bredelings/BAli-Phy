{-# LANGUAGE DuplicateRecordFields, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data A = A { shared :: Int, onlyA :: Int }
data B = B { shared :: Int, onlyB :: Int }

setShared p = p { shared = 10 }

main = print (setShared (A 1 2))
