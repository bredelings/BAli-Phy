{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data A = A { shared :: Int }
data B = B { shared :: Int }

main = print (shared (A 1))
