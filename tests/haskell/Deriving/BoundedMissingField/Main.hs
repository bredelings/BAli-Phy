{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Enum
import Compiler.IO (IO(IO))

data Foo = Foo
data T a = T a Foo deriving Bounded

main = IO (\s -> (s, ()))
