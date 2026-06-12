{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

import RecordSupport (Point(..))

main = print (x (Point 1 2))
