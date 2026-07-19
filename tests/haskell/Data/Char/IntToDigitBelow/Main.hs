{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Char (intToDigit)
import System.IO (print)

main = print (intToDigit (-1))
