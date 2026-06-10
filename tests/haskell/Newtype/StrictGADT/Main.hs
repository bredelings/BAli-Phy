{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))

newtype StrictGADT where
  StrictGADT :: !Int -> StrictGADT

main = IO (\s -> (s, ()))
