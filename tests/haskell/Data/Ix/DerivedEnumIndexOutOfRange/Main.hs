{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Eq
import Data.Ix
import Data.Ord
import System.IO (print)

data Color = Red | Green | Blue deriving Eq deriving Ord deriving Ix

main = print (index (Red,Green) Blue)
