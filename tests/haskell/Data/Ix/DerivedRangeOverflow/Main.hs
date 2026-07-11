{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import Data.Ix
import Data.Ord
import System.IO (print)

data Grid = Grid Int Int deriving Eq deriving Ord deriving Ix

main = print (rangeSize (Grid 0 0, Grid 65535 65535))
