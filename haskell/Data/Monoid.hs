{-# LANGUAGE NoImplicitPrelude #-}
module Data.Monoid where

import Data.Semigroup

class Semigroup a => Monoid a where
    mempty :: a

    mappend :: a -> a -> a
    mappend = (<>)

    mconcat :: [a] -> a
    mconcat [] = mempty
    mconcat (x:xs) = x <> (mconcat xs)

instance Monoid () where
    mempty = ()
    mconcat _ = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty = (mempty, mempty)
