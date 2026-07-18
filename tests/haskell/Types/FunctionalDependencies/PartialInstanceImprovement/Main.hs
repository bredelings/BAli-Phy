{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies, UndecidableInstances #-}

data Box a = Box a
data A = A
data B = B
data R = R

class D b a | b -> a where
    extract :: b -> a

instance D B A where
    extract _ = A

class C a b | b -> a where
    convert :: b -> a

instance D b a => C (Box a) b where
    convert x = Box (extract x)

class Q a where
    consume :: a -> R

instance Q (Box A) where
    consume _ = R

partiallyMatchedInstance :: R
partiallyMatchedInstance = consume (convert B)
