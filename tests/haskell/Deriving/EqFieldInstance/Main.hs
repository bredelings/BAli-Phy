module Main where

data Foo = Foo

instance Eq Foo where
  Foo == Foo = True

data T a = T a Foo deriving Eq

ok = T 1 Foo == T 1 Foo

main = print (if ok then 1 else 0)
