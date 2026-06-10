{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import System.IO (print)

class Twice a where
  twice :: a -> a

instance Twice Int where
  twice x = x + x

newtype Age = Age Int deriving Twice

unAge (Age x) = x

ok = unAge (twice (Age 21)) == 42

main = print (if ok then (1 :: Int) else 0)
