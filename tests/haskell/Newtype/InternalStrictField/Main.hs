{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.IO (IO(IO))
import Data.Maybe (Maybe)

newtype InternalStrict = InternalStrict (Maybe !Int)

main = IO (\s -> (s, ()))
