{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

data family F a
data Other = Other

newtype instance F Int where
  MkF :: Int -> F Other

main = IO (\s -> (s, ()))
