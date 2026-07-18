{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Data.Bool
import Data.Eq
import System.IO (print)
import Text.Show

-- Check the negative, zero, and positive branches of each scalar Num implementation.
main = print (if
    signum (-7 :: Integer) == -1
    && signum (0 :: Integer) == 0
    && signum (7 :: Integer) == 1
    && signum (-7 :: Int) == -1
    && signum (0 :: Int) == 0
    && signum (7 :: Int) == 1
    && signum (-0.5 :: Double) == -1.0
    && signum (0.0 :: Double) == 0.0
    && signum (0.5 :: Double) == 1.0
    then (1 :: Int) else 0)
