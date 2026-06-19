{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import System.IO (print)

class Marker a

instance Marker Int

requiresMarker :: Marker a => a -> Int
requiresMarker _ = 42

ok = requiresMarker (7 :: Int) == 42

main = print (if ok then (1 :: Int) else 0)
