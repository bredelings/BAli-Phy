{-# LANGUAGE NoImplicitPrelude #-}
module Data.Foldable where

import Data.Eq
import Data.Ord
import Data.Maybe
import Data.Function
import Data.Semigroup
import Data.Monoid
import Compiler.Num -- for Num
import Compiler.Error -- for error
import qualified Data.OldList as L
import qualified Foreign.Vector as V

infix 4 `elem`, `notElem`

class Foldable t where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap' :: Monoid m => (a -> m) -> t a -> m

    fold = L.foldr (<>) mempty . toList
    foldMap f = fold . L.map f . toList
    foldMap' f = L.foldl' (<>) mempty . L.map f . toList

    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl' :: (b -> a -> b) -> b -> t a -> b

    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a
    toList :: t a -> [a]
    null :: t a -> Bool
    length :: t a -> Int
    elem :: Eq a => a -> t a -> Bool

    maximum :: Ord a => t a -> a
    minimum :: Ord a => t a -> a
    sum :: Num a => t a -> a
    product :: Num a => t a -> a

    foldr f z  = L.foldr f z . toList
    foldr' f z = error "foldr' undefined"
    foldl f z  = L.foldl f z . toList
    foldl' f z = L.foldl' f z . toList

    foldr1 f = L.foldr1 f . toList
    foldl1 f = L.foldl1 f . toList
    length = L.length . toList
    null = L.null . toList
    elem x = L.elem x . toList

    maximum = foldl1 (max)
    minimum = foldl1 (min)
    sum     = foldl (+) 0
    product = foldl (*) 1

-- List of functions should go here...

instance Foldable [] where
    toList xs = xs

instance Foldable V.EVector where
    toList = V.vectorToList
    length = V.vector_size
    null v = length v == 0

notElem x c = not (elem x c)

concat xs   = foldr (L.++) [] xs

concatMap f xs = foldr (\x ys -> f x L.++ ys) [] xs

and         =  foldr (&&) True

or          =  foldr (||) False

all p       =  foldr (\x y -> p x && y) True

any p       =  foldr (\x y -> p x || y) False

find p xs = foldr found Nothing xs where
    found x f = if p x
                then Just x
                else f
