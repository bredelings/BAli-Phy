{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

newtype Bad where
  Bad :: a -> Bad

main = IO (\s -> (s, ()))
