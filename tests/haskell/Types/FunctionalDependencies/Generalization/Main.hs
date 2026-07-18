{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data B = B
data Pair a b = Pair a b

class C a b | a -> b where
    convert :: a -> b

generalized x =
    let inner y = Pair (convert x) y
    in Pair (inner A) (inner B)
