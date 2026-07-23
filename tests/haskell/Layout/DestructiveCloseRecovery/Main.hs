{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Control.Monad (return)
import System.IO (IO, print)

main :: IO ()
main = do { let x = 1; print x; return () }
