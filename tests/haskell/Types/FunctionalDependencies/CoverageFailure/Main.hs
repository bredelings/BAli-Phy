{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

class C a b | a -> b

instance C Int a
