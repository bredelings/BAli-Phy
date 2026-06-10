module Main where

data Point = Point { x :: Int, y :: Int }

p = (Point 1 2) { z = 3 }

main = print (x p)
