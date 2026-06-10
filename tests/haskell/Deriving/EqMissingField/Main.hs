module Main where

data Foo = Foo
data T a = T a Foo deriving Eq

main = print 1
