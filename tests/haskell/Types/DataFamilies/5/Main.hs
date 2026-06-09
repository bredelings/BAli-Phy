{-# LANGUAGE NoImplicitPrelude #-}

data family F a

data instance F a = MkAny a

data instance F Int = MkInt Int
