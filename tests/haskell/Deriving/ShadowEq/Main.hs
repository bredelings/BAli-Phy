{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

class Eq a

data T = T deriving Eq

main = IO (\s -> (s, ()))
