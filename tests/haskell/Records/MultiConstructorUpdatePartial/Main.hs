{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data T = Both { x :: Int, y :: Int }
       | OnlyX { x :: Int }

setY :: T -> Int -> T
setY t n = t { y = n }

readBoth :: T -> (Int, Int)
readBoth (Both a b) = (a, b)
readBoth (OnlyX a) = (a, 0)

main = print (readBoth (setY (Both 1 2) 9))
