module Main where

data Foo = Foo
data T a = T a Foo deriving Show

main = print 1
