{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

import RecordSupport (Point(..), readPoint)

setY p b = p { y = b }

main = print (readPoint (setY (Point { x = 1, y = 2 }) 7))
