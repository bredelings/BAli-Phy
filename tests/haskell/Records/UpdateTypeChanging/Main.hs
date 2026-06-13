{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import Data.Bool
import System.IO (print)

data Box a = Box { item :: a, tag :: Int }

setItem :: Box Int -> Box Bool
setItem b = b { item = True }

main = print (tag (setItem (Box 3 7)))
