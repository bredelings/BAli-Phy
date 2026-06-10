{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

newtype Bad a where
  Bad :: Int -> Bad Int

main = IO (\s -> (s, ()))
