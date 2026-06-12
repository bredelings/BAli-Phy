{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data A = A { shared :: Int, onlyA :: Int }
data B = B { shared :: Int, onlyB :: Int }

fromA A { shared, onlyA } = shared + onlyA
fromB B { shared, onlyB } = shared + onlyB

main = print (fromA (A { shared = 2, onlyA = 3 }), fromB (B { shared = 5, onlyB = 7 }))
