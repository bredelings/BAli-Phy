{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data A = A { shared :: Int, onlyA :: Int }
data B = B { shared :: Int, onlyB :: Int }

readA A { shared, onlyA } = shared + onlyA
readB B { shared, onlyB } = shared + onlyB

setSharedA :: A -> A
setSharedA a = a { shared = 10 }

setSharedBySibling :: A -> A
setSharedBySibling a = a { shared = 20, onlyA = 1 }

main = print (readA (setSharedA (A 2 3)), readA (setSharedBySibling (A 4 5)), readB ((B 6 7) { shared = 30 }))
