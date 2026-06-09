{-# LANGUAGE NoImplicitPrelude #-}

data family F (c :: * -> *) :: *

data instance F Int = FInt
