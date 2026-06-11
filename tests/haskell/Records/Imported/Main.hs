{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

import RecordSupport (Point(..), shift)

main = print (x (Point { y = 4, x = 3 }), y (shift (Point 1 2)))
