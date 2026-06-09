{-# LANGUAGE NoImplicitPrelude #-}

type family G a

data family F a

data instance F (G a) = FG
data instance F Int = FI
