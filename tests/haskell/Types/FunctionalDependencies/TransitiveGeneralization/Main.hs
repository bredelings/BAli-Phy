{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data B = B
data Pair a b = Pair a b

class C a b | a -> b where
    first :: a -> b

class D b c | b -> c where
    second :: b -> c

transitive x =
    let inner y = Pair (first x) (Pair (second (first x)) y)
    in Pair (inner A) (inner B)
