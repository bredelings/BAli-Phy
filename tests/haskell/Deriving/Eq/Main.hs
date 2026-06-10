module Main where

data Color = Red | Blue deriving Eq
data Box a = Box a | Empty deriving Eq

ok = Red == Red
  && not (Red == Blue)
  && Box 3 == Box 3
  && not (Box 3 == Box 4)
  && (Empty :: Box Int) == Empty

main = print (if ok then 1 else 0)
