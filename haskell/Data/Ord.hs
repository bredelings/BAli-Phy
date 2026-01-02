{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ord (module Data.Eq,
                 Ordering (..),
                 Ord(..))
where

import Data.Eq

data Ordering = LT | EQ | GT

instance Eq Ordering where
    EQ == EQ = True
    LT == LT = True
    GT == GT = True
    _  == _  = False

instance Ord Ordering where
    compare LT LT = EQ
    compare LT _  = LT

    compare EQ LT = GT
    compare EQ EQ = EQ
    compare EQ GT = LT

    compare GT GT = EQ
    compare GT _  = GT

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

    x <  y = case compare x y of {LT -> True ; _ -> False}
    x >  y = not (x <= y)
    x >= y = x > y || x == y
    x <= y = x < y || x == y

    {- For GHC, you can specify either compare or (<=).
       Here you can specify either compare or (<). -}

foreign import ecall "Prelude:" lessthan_char :: Char -> Char -> Bool
foreign import ecall "Prelude:" lessthan_int :: Int -> Int -> Bool
foreign import ecall "Prelude:" lessthan_integer :: Integer -> Integer -> Bool
foreign import ecall "Prelude:" lessthan_double :: Double -> Double -> Bool

instance Ord Char where
    (<) = lessthan_char

instance Ord Int where
    (<) = lessthan_int 

instance Ord Integer where
    (<) = lessthan_integer

instance Ord Double where
    (<) = lessthan_double

instance Ord a => Ord [a] where
    compare []     []      = EQ
    compare []     (_:_)   = LT
    compare (_:_)  []      = GT
    compare (x:xs) (y:ys)  = case compare x y of LT -> LT
                                                 GT -> GT
                                                 EQ -> compare xs ys
    x < y = compare x y == LT
    x > y = compare x y == GT

instance (Ord a, Ord b) => Ord (a,b) where
    compare (x1,y1) (x2,y2) = let c1 = compare x1 x2
                              in case c1 of
                                   EQ -> compare y1 y2
                                   _  -> c1
