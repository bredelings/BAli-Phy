module Main where

data Point = Point { x :: Int, y :: Int }

bad (Point { z = a }) = a

main = print (bad (Point 1 2))
