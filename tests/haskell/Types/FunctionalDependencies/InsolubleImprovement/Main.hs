{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

data A = A
data B = B
data D = D

class C a b | a -> b where
    convert :: a -> b

bad :: C A B => A -> D
bad = convert
