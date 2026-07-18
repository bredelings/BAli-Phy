{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data D = D
data R = R

class C a b | b -> a where
    convert :: b -> a

class Q a where
    consume :: a -> R

instance Q A where
    consume _ = R

improvedLocally :: C A D => R
improvedLocally = consume (convert D)
