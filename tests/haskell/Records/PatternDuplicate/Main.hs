module Main where

data Point = Point { x :: Int, y :: Int }

bad (Point { x = a, x = b }) = a + b

main = print (bad (Point 1 2))
