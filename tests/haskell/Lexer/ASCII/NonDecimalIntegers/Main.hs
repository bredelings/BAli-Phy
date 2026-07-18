{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import System.IO (print)

main = do
  print (0x1_0 :: Integer)
  print (0o1_0 :: Integer)
  print (0x1_0000_0000_0000_0000 :: Integer)
