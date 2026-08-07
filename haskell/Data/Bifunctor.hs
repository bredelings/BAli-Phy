{-# LANGUAGE NoImplicitPrelude #-}
module Data.Bifunctor (Bifunctor(..)) where

import Data.Either
import Data.Function

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    first :: (a -> b) -> p a c -> p b c
    second :: (b -> c) -> p a b -> p a c

    bimap f g = first f . second g
    first f = bimap f id
    second = bimap id

instance Bifunctor Either where
    bimap f _ (Left x) = Left (f x)
    bimap _ g (Right y) = Right (g y)

instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)
