{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))
import Data.Eq (Eq)

newtype Bad where
  Bad :: Eq a => a -> Bad

main = IO (\s -> (s, ()))
