{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AssocReExportSupport (C(..))
import Compiler.Num
import Data.Eq
import System.IO (print)

data Box = Box

instance C Box where
    type T Box = Int
    data D Box = MkD Int

useT :: T Box -> Int
useT x = x + 1

useD :: D Box -> Int
useD (MkD x) = x

ok = useT 41 + useD (MkD 0) == 42

main = print (if ok then (1 :: Int) else 0)
