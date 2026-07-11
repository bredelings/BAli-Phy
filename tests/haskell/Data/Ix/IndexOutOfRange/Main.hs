{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Ix
import System.IO (print)

bounds :: ((Int,Int),(Int,Int))
bounds = ((0,0),(1,1))

main = print (index bounds (0,2))
