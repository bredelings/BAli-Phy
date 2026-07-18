{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

class C a b | a -> b where
    keep :: a -> a

instance C Int Double where
    keep x = x

instance C Char Double where
    keep x = x
