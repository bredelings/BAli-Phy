module Main where

data Foo = Foo
data T a = T a Foo deriving Bounded

main = print 1
