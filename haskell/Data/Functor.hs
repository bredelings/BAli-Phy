{-# LANGUAGE NoImplicitPrelude #-}
module Data.Functor where

import Data.Function
import Data.Tuple (fst, snd)

class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a

    (<$) = fmap . const

($>) = flip (<$)

f <$> x = fmap f x

(<&>) = flip fmap

void x = () <$ x

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = (f x):(fmap f xs)

