{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

import ForeignRecord (Foreign(Foreign), makeForeign, readForeign)
import LocalRecord (Local(..))

updateForeign :: Foreign -> Foreign
updateForeign f = f { x = 9 }

main = print (readForeign (updateForeign (makeForeign 1)))
