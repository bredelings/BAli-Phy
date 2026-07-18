{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

class C a b c | a -> b, a -> c where
    keep :: a -> a

instance C Int Double Char where
    keep x = x

instance C Int Char Double where
    keep x = x
