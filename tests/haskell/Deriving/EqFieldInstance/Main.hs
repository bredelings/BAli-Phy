{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import System.IO (print)

data Foo = Foo

instance Eq Foo where
  Foo == Foo = True

data T a = T a Foo deriving Eq

ok = T 1 Foo == T 1 Foo

main = print (if ok then (1 :: Int) else 0)
