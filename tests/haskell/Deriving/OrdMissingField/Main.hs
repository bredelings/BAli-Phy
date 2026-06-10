{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Ord
import Compiler.IO (IO(IO))

data Foo = Foo deriving Eq
data T a = T a Foo deriving Eq deriving Ord

main = IO (\s -> (s, ()))
