{-# LANGUAGE NoImplicitPrelude #-}
module Data.Semigroup where

import Data.List.NonEmpty
import Data.Ord
import Data.Function
import Data.Functor
import Compiler.Num

infixr 6 <>

class Semigroup a where
    (<>) :: a -> a -> a
    x <> y = sconcat ( x :| [y] )

    sconcat :: NonEmpty a -> a
    sconcat (x :| xs) = go x xs where
                  go x [] = x
                  go x (y:ys) = x <> go y ys

--  We probably need to split the NonEmpty definition into a different file
--  (maybe Compiler.NonEmpty) to avoid module loops.

--  stimes :: Integral b => b -> a -> a

instance Semigroup () where
    _ <> _ = ()
    sconcat _ = ()

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (x,y) <> (x',y') = (x <> x', y <> y')

instance Semigroup Ordering where
    EQ <> y = y
    x  <> _ = x

data Min a = Min {getMin :: a}

data Max a = Max {getMax :: a}

instance Ord a => Semigroup (Min a ) where
    Min x <> Min y = Min (min x y)
--    stimes = stimesIdempotent

instance Functor Min where
    fmap f (Min x) = Min (f x)

instance Num a => Num (Min a) where
    (Min x) + (Min y) = Min (x + y)
    (Min x) * (Min y) = Min (x * y)
    (Min x) - (Min y) = Min (x - y)
    negate (Min x) = Min (negate x)
    abs    (Min x) = Min (abs x)
    signum (Min x) = Min (signum x)
    fromInteger = Min . fromInteger

instance Ord a => Semigroup (Max a ) where
    Max x <> Max y = Max (min x y)
--    stimes = stimesIdempotent

instance Functor Max where
    fmap f (Max x) = Max (f x)

instance Num a => Num (Max a) where
    (Max x) + (Max y) = Max (x + y)
    (Max x) * (Max y) = Max (x * y)
    (Max x) - (Max y) = Max (x - y)
    negate (Max x) = Max (negate x)
    abs    (Max x) = Max (abs x)
    signum (Max x) = Max (signum x)
    fromInteger = Max . fromInteger

data Arg a b = Arg a b

type ArgMin a b = Min (Arg a b)
type ArgMax a b = Max (Arg a b)

instance Functor (Arg a) where
    fmap f (Arg x y) = Arg x (f y)

--instance Foldable (Arg a) where
--    foldMap f (Arg _ y) = f y

--instance Traversable (Arg a) where
--    traverse f (Arg x y) = Arg x <$> f y

instance Eq a => Eq (Arg a b) where
    Arg x1 y1 == Arg x2 y2 = x1 == x2

instance Ord a => Ord (Arg a b) where
    Arg x1 _ `compare` Arg x2 _ = compare x1 x2
    min x@(Arg a _) y@(Arg b _) | a <= b    = x
                                | otherwise = y
    max x@(Arg a _) y@(Arg b _) | a >= b    = x
                                | otherwise = y
