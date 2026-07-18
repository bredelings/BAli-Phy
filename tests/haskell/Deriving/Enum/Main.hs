{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Enum
import Compiler.Num
import Data.Eq
import Data.List (take)
import Text.Show
import System.IO (print)

data Color = Red | Blue | Green deriving Eq deriving Show deriving Enum

ok = fromEnum Red == 0
  && fromEnum Green == 2
  && toEnum 1 == Blue
  && succ Red == Blue
  && pred Green == Blue
  && [Red .. Green] == [Red, Blue, Green]
  && [Red, Blue .. Green] == [Red, Blue, Green]
  && [Green, Blue .. Red] == [Green, Blue, Red]
  && take 3 [Red, Red ..] == [Red, Red, Red]
  && take 3 [Blue, Blue ..] == [Blue, Blue, Blue]

main = print (if ok then (1 :: Int) else 0)
