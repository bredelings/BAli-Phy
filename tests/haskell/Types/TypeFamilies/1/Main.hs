{-# LANGUAGE NoImplicitPrelude #-}

class Container c where
    type Element c
    type instance Element c = c

type family Elem c

type instance Elem [c] = c

type family F (c :: * -> *) :: *

type instance F [] = [Int]

type instance F Int = [Int] -- this should fail!
