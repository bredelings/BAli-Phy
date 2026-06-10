module Main where

data Foo = Foo deriving Eq
data T a = T a Foo deriving Eq deriving Ord

main = print 1
