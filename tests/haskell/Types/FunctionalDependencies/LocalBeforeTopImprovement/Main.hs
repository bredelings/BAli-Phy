{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data B = B
data D = D
data R = R

class C a b | a -> b where
    convert :: a -> b

instance C A B where
    convert _ = B

class Q a where
    consume :: a -> R

instance Q D where
    consume _ = R

prefersLocalConstraint :: C A D => R
prefersLocalConstraint = consume (convert A)
