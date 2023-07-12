{-# LANGUAGE NoImplicitPrelude #-}
module Data.Functor where

import Data.Function
import Data.Tuple (fst, snd)
import Data.Maybe

class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a

    infixl 4 <$
    (<$) = fmap . const

infixl 4 $>
($>) = flip (<$)

infixl 4 <$>
f <$> x = fmap f x

infixl 1 <&>
(<&>) = flip fmap

void x = () <$ x

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = (f x):(fmap f xs)

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

instance Functor ((->) r) where
  fmap = (.)
