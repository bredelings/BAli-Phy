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

instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

instance Functor ((,,) a b) where
    fmap f (x1,x2,x3) = (x1,x2,f x3)

instance Functor ((,,,) a b c) where
    fmap f (x1,x2,x3,x4) = (x1,x2,x3,f x4)

instance Functor ((,,,,) a b c d) where
    fmap f (x1,x2,x3,x4,x5) = (x1,x2,x3,x4,f x5)

instance Functor ((,,,,,) a b c d e) where
    fmap f (x1,x2,x3,x4,x5,x6) = (x1,x2,x3,x4,x5,f x6)

instance Functor ((,,,,,,) a b c d e f) where
    fmap f (x1,x2,x3,x4,x5,x6,x7) = (x1,x2,x3,x4,x5,x6,f x7)
