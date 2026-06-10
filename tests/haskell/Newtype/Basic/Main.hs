module Main where

newtype Age = Age Int deriving Eq deriving Ord

unAge (Age x) = x

ok = Age 1 < Age 2
  && Age 3 == Age 3
  && unAge (Age 5) == 5

main = print (if ok then 1 else 0)
