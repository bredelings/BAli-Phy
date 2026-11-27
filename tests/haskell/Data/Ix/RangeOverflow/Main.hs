{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Ix
import System.IO (print)

bounds :: ((Int,Int),(Int,Int))
bounds = ((0,0),(4294967295,4294967295))

main = print (rangeSize bounds)
