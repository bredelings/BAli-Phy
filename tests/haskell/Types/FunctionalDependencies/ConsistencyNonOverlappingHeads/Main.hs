{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data X
data Y

class C a b marker | a -> b where
    keep :: marker -> marker

instance C Int Double X where
    keep x = x

instance C Int Char Y where
    keep x = x
