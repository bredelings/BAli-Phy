{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import ImportQualifiedPostSupport qualified as Support
import Compiler.Num
import System.IO (print)

main = print (Support.value + Support.other)
