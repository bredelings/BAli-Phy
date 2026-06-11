{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Enum
import Compiler.IO (IO(IO))
import Compiler.Num

data T = T Int deriving Enum

main = IO (\s -> (s, ()))
