{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Eq
import Compiler.IO (IO(IO))

data Foo = Foo
data T a = T a Foo deriving Eq

main = IO (\s -> (s, ()))
