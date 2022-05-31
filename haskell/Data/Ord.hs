{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ord (module Data.Eq,
                 Ord,
                (<),
                (<=),
                (>),
                (>=),
                compare,
                min,
                max)
where

import Data.Eq

data Ordering = EQ | LT | GT

instance Eq Ordering where
    EQ == EQ = True
    LT == LT = True
    GT == GT = True
    _  == _  = False

infix 4 <, <=, >, >=

class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<), (>), (>=), (<=) :: a -> a -> Bool
    min, max :: a -> a -> a

    min x y = if (x <= y) then x else y
    max x y = if (x >= y) then x else y

    compare x y | x <  y    = LT
                | x == y    = EQ
                | otherwise = GT

    x <  y = not (x >= y)
    x >  y = not (x <= y)
    x >= y = x > y || x == y
    x <= y = x < y || x == y

foreign import bpcall "Prelude:" lessthan_char :: Char -> Char -> Bool
foreign import bpcall "Prelude:" lessthan_int :: Int -> Int -> Bool
foreign import bpcall "Prelude:" lessthan_double :: Double -> Double -> Bool

instance Ord Char where
    (<) = lessthan_char

instance Ord Int where
    (<) = lessthan_int 

instance Ord Double where
    (<) = lessthan_double

