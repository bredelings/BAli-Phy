{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

class C a b | a -> b where
    empty :: a
