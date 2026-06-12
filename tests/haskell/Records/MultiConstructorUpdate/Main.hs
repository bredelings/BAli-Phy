{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Shape = Point { x :: Int, y :: Int }
           | Segment { x :: Int, y :: Int, len :: Int }

setX s a = s { x = a }

readShape (Point { x = a, y = b }) = (a, b)
readShape (Segment { x = a, y = b, len = c }) = (a + c, b)

main = print (readShape (setX (Point 1 2) 9), readShape (setX (Segment 3 4 5) 8))
