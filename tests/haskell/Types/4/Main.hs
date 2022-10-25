{-# LANGUAGE NoImplicitPrelude #-}

data T a = T1 a | (a ~ Int) => T2

f (T1 x) = x
f T2     = 2#

g = f (T1 "hello")
