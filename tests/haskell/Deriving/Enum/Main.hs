module Main where

data Color = Red | Blue | Green deriving Eq deriving Show deriving Enum

ok = fromEnum Red == 0
  && fromEnum Green == 2
  && toEnum 1 == Blue
  && succ Red == Blue
  && pred Green == Blue
  && [Red .. Green] == [Red, Blue, Green]
  && [Red, Blue .. Green] == [Red, Blue, Green]

main = print (if ok then 1 else 0)
