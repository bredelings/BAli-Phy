module Main where

data family F a

newtype instance F Int = BadF Int Int

main = print 1
