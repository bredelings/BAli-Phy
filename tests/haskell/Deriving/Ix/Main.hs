{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Enum
import Compiler.Num
import Data.Eq
import Data.Ix
import Data.Ord
import Text.Show
import System.IO (print)

data Color = Red | Blue | Green deriving Eq deriving Ord deriving Show deriving Ix
data Point = Point Color Int deriving Eq deriving Ord deriving Show deriving Ix
data Box a = Box a deriving Eq deriving Ord deriving Show deriving Ix
data Pixel = Pixel { pixelColor :: Color, pixelIndex :: Int } deriving Eq deriving Ord deriving Show deriving Ix
newtype Age = Age Int deriving Eq deriving Ord deriving Show deriving Ix

ok = range (Red, Green) == [Red, Blue, Green]
  && range (Blue, Red) == []
  && index (Red, Green) Blue == 1
  && inRange (Red, Blue) Blue
  && not (inRange (Red, Blue) Green)
  && rangeSize (Red, Green) == 3
  && rangeSize (Green, Red) == 0
  && range (Point Red 1, Point Blue 2) == [Point Red 1, Point Red 2, Point Blue 1, Point Blue 2]
  && index (Point Red 1, Point Blue 2) (Point Blue 1) == 2
  && inRange (Point Red 1, Point Blue 2) (Point Blue 2)
  && not (inRange (Point Red 1, Point Blue 2) (Point Green 1))
  && rangeSize (Point Red 1, Point Blue 2) == 4
  && range (Box Red, Box Blue) == [Box Red, Box Blue]
  && index (Box Red, Box Green) (Box Blue) == 1
  && range (Pixel Red 1, Pixel Blue 2) == [Pixel Red 1, Pixel Red 2, Pixel Blue 1, Pixel Blue 2]
  && range (Age 2, Age 4) == [Age 2, Age 3, Age 4]
  && index (Age 2, Age 4) (Age 3) == 1

main = print (if ok then (1 :: Int) else 0)
