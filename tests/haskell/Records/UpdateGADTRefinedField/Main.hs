{-# LANGUAGE GADTs, NoImplicitPrelude, NoFieldSelectors #-}

module Main where

import Compiler.Num
import Data.Bool
import System.IO (print)

data G a where
    MkG :: { item :: a, tag :: Int } -> G [a]

setItem :: G [Int] -> G [Bool]
setItem g = g { item = True }

readTag :: G a -> Int
readTag (MkG { tag = n }) = n

main = print (readTag (setItem (MkG { item = 3, tag = 8 })))
