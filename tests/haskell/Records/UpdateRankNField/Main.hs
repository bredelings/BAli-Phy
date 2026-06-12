{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module Main where

import Compiler.Num
import System.IO (print)

data R = R { poly :: forall a. a -> a, n :: Int }

setPoly :: R -> R
setPoly r = r { poly = \x -> x }

readR :: R -> (Int, Char)
readR (R { poly = f, n = k }) = (f k, f 'q')

main = print (readR (setPoly (R { poly = \x -> x, n = 7 })))
