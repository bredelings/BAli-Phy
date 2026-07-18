{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

module Support where

data A = A
data D = D

class C a b | b -> a where
    convert :: b -> a

instance C A D where
    convert _ = A
