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

instance Bifunctor ((,,) a) where
    bimap f g (x1,x2,x3) = (x1,f x2,g x3)

instance Bifunctor ((,,,) a b) where
    bimap f g (x1,x2,x3,x4) = (x1,x2,f x3,g x4)

instance Bifunctor ((,,,,) a b c) where
    bimap f g (x1,x2,x3,x4,x5) = (x1,x2,x3,f x4,g x5)

instance Bifunctor ((,,,,,) a b c d) where
    bimap f g (x1,x2,x3,x4,x5,x6) = (x1,x2,x3,x4,f x5,g x6)

instance Bifunctor ((,,,,,,) a b c d e) where
    bimap f g (x1,x2,x3,x4,x5,x6,x7) = (x1,x2,x3,x4,x5,f x6,g x7)
