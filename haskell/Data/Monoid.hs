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

instance (Monoid a,Monoid b,Monoid c) => Monoid (a,b,c) where
    mempty = (mempty,mempty,mempty)

instance (Monoid a,Monoid b,Monoid c,Monoid d) => Monoid (a,b,c,d) where
    mempty = (mempty,mempty,mempty,mempty)

instance (Monoid a,Monoid b,Monoid c,Monoid d,Monoid e) => Monoid (a,b,c,d,e) where
    mempty = (mempty,mempty,mempty,mempty,mempty)
