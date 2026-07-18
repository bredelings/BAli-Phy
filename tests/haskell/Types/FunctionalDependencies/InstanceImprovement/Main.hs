{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data D = D
data R = R

class C a b | b -> a where
    convert :: b -> a

instance C A D where
    convert _ = A

class Q a where
    consume :: a -> R

instance Q A where
    consume _ = R

improvedFromInstance :: R
improvedFromInstance = consume (convert D)
