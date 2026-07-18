{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data D = D
data R = R
data Pair a = Pair a a

class C a b | b -> a where
    convert :: b -> a

class Q a where
    consume :: a -> R

instance Q (Pair A) where
    consume _ = R

sameResults :: C A D => R
sameResults = consume (Pair (convert D) (convert D))
