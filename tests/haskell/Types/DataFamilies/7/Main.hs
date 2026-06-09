{-# LANGUAGE NoImplicitPrelude #-}

data family F a

data instance F [a] where
    MkBad :: F Int
