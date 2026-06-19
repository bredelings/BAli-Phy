{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AssocShadowReExportSupport (C(..))
import Compiler.Num
import Data.Eq
import System.IO (print)

data Box = Box

instance C Box where
    type T Box = Int

useT :: T Box -> Int
useT x = x + 1

ok = useT 41 == 42

main = print (if ok then (1 :: Int) else 0)
