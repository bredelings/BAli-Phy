{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Ix
import System.IO (print)

bounds :: ((Int,Int),(Int,Int))
bounds = ((0,0),(65535,65535))

main = print (rangeSize bounds)
