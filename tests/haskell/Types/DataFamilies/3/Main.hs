{-# LANGUAGE NoImplicitPrelude #-}

data family F a b

data instance F Int = MkF
