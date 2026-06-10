module Main where

import Data.Ix

data Color = Red | Blue | Green deriving Eq deriving Ord deriving Show deriving Ix

ok = range (Red, Green) == [Red, Blue, Green]
  && range (Blue, Red) == []
  && index (Red, Green) Blue == 1
  && inRange (Red, Blue) Blue
  && not (inRange (Red, Blue) Green)
  && rangeSize (Red, Green) == 3
  && rangeSize (Green, Red) == 0

main = print (if ok then 1 else 0)
