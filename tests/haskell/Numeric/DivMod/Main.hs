{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Classes
import Compiler.Integral
import Compiler.Num
import Data.Eq
import System.IO (print)
import Text.Show

intCases = [(5,3), (5,-3), (-5,3), (-5,-3), (6,-3), (-6,3)] :: [(Int,Int)]
integerCases = [(5,3), (5,-3), (-5,3), (-5,-3), (6,-3), (-6,3)] :: [(Integer,Integer)]

-- Check every sign combination, exact division, and the div/mod reconstruction law.
main = do
  print [(x `div` y, x `mod` y, x == (x `div` y) * y + x `mod` y) | (x,y) <- intCases]
  print [(x `div` y, x `mod` y, x == (x `div` y) * y + x `mod` y) | (x,y) <- integerCases]
