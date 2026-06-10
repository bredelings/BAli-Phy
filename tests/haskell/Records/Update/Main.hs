module Main where

data Point = Point { x :: Int, y :: Int }

setX p x = p { x }

main = print (y ((Point 3 4) { y = 9 }), x (setX (Point 3 4) 8))
