{-# LANGUAGE NoImplicitPrelude #-}

data A = A
data Wrap a = Wrap a
data Box = Box (Wrap !A)
