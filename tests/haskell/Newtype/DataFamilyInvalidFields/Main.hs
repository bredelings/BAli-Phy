{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

data family F a

newtype instance F Int = BadF Int Int

main = IO (\s -> (s, ()))
