{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Semigroup
import System.IO (print)

main = print (getMin (Min 7 <> Min 2), getMax (Max 7 <> Max 2))
