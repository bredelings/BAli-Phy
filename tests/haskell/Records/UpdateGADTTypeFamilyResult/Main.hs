{-# LANGUAGE GADTs, TypeFamilies, NoFieldSelectors, NoImplicitPrelude #-}

module Main where

import Compiler.Fractional
import Compiler.Num
import System.IO (print)

type family F a

data G a b where
    MkG :: { bar :: F a, tag :: Int } -> G a Int

setBar :: F Int -> G Double b -> G Int b
setBar i g = g { bar = i }

main = print (0 :: Int)
