{-# LANGUAGE NoImplicitPrelude #-}

class Container c where
    type Element c
    type instance Element c = c

type family Elem c

type instance Elem [c] = c

type family F (c :: * -> *) :: *

type instance F [] = [Int]

instance Container [a] where
    type Element [a] = a

class Asdf a b where
    type Foo a x

instance Asdf [y] z where
    type Foo [y] b = Int

type family Closed a where
    Closed Int = Int
    Closed Double = Char


type G :: Type -> Type
type family G c

type instance G [] = [Int]
type instance G ((,) a) = (a,a)

