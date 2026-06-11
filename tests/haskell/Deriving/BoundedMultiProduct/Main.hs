{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Enum
import Compiler.IO (IO(IO))
import Compiler.Num

data T = A Int | B Int deriving Bounded

main = IO (\s -> (s, ()))
