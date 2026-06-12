{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)
import RecordSupport (A(..), B(..))

readA A { shared, onlyA } = shared + onlyA
readB B { shared, onlyB } = shared + onlyB

setSharedA :: A -> A
setSharedA a = a { shared = 10 }

main = print (readA (setSharedA (A 2 3)), readB ((B 4 5) { shared = 20 }))
