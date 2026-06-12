{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main where

import Compiler.Num
import System.IO (print)

import RecordSupport (Point(..))

readPoint (Point { y = b, .. }) = (x, b, z)

main = print (readPoint (Point 1 2 3))
