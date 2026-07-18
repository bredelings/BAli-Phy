{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

class C a b | a -> b

instance C [a] a
