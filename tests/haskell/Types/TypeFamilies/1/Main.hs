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
    type Foo y z = y

instance Asdf [y] z where
    type Foo [y] b = Int

instance Asdf Int z
-- We should get Foo Int x = Int

type family Closed a where
    Closed Int = Int
    Closed Double = Char


type G :: (Type -> Type) -> Type
type family G c

type instance G [] = [Int]
type instance G ((,) a) = (a,a)

