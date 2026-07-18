{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data B = B
data Pair a b = Pair a b

class C a b | a -> b where
    convert :: a -> b

nested x =
    let middle z =
            let inner y = Pair (convert x) y
            in Pair (inner z) (inner B)
    in middle A
