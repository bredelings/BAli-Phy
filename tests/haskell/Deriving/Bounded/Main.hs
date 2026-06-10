module Main where

data Color = Red | Blue | Green deriving Eq deriving Bounded
data Box a = Box a deriving Eq deriving Bounded

ok = minBound == Red
  && maxBound == Green
  && minBound == Box Red
  && maxBound == Box Green

main = print (if ok then 1 else 0)
