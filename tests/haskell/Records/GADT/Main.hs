{-# LANGUAGE GADTs, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Box a where
    Box :: { value :: a, tag :: Int } -> Box a

readBox :: Box Int -> (Int, Int)
readBox (Box { value = x, tag = n }) = (x, n)

bumpTag :: Box Int -> Box Int
bumpTag b = b { tag = tag b + 1 }

main = print (readBox (bumpTag (Box { tag = 7, value = 3 })), value (Box { tag = 9, value = (4 :: Int) }))
