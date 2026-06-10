{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

newtype Bad = Bad Int Int

main = IO (\s -> (s, ()))
