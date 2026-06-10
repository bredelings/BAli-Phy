{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

newtype Strict = Strict !Int

main = IO (\s -> (s, ()))
