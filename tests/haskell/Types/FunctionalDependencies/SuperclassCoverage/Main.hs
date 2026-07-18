{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies, UndecidableInstances #-}

class E a b | a -> b
class E a b => D a b
class C a b | a -> b where
    keep :: a -> a

instance D a b => C [a] [b] where
    keep x = x
