{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

module Support where

class C a b | a -> b where
    keep :: a -> a

instance C Int Double where
    keep x = x
