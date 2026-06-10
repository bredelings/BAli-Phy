module Main where

data Color = Red | Blue | Green deriving Eq deriving Ord
data Box a = Empty | Box a | Pair a a deriving Eq deriving Ord

ok = compare Red Red == EQ
  && compare Red Blue == LT
  && compare Green Blue == GT
  && Empty < Box 0
  && Box 1 < Box 2
  && Box 2 < Pair 0 0
  && Pair 1 2 < Pair 1 3
  && max Red Blue == Blue

main = print (if ok then 1 else 0)
