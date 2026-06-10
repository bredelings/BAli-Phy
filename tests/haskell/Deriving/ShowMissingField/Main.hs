{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Text.Show
import Compiler.IO (IO(IO))

data Foo = Foo
data T a = T a Foo deriving Show

main = IO (\s -> (s, ()))
