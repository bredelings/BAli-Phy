{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

class D a b | a -> b
class C a b | a -> b where
    keep :: a -> a

instance D a b => C [a] [b] where
    keep x = x
