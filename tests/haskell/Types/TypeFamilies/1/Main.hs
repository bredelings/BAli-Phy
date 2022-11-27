{-# LANGUAGE NoImplicitPrelude #-}

class Container c where
    type Element c
    type instance Element c = c

type family Elem c

type instance Elem [c] = c

type family F (c :: * -> *) :: *

type instance F [] = [Int]

type instance F Int = [Int] -- this should fail because the kind is wrong!

instance Container [a] where
    type Element [a] = a

class Asdf a b where
    type Foo a x

instance Asdf [y] z where
    type Foo [y] b = Int
