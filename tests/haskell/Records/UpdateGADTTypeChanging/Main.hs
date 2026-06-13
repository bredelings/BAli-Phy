{-# LANGUAGE GADTs, NoImplicitPrelude, NoFieldSelectors #-}

module Main where

import Compiler.Num
import Compiler.Fractional
import System.IO (print)

data G a b where
    MkG :: { bar :: a, tag :: Int } -> G a Int

setBar :: G Double b -> G Int b
setBar g = g { bar = 3 }

getTag :: G a b -> Int
getTag (MkG { tag = n }) = n

main = print (getTag (setBar (MkG { bar = 1.5, tag = 8 })))
