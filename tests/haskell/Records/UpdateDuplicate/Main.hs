module Main where

data Point = Point { x :: Int, y :: Int }

p = (Point 1 2) { x = 3, x = 4 }

main = print (x p)
