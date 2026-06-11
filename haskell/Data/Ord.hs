{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ord (module Data.Eq,
                 Ordering (..),
                 Ord(..))
where

import Data.Eq

data Ordering = LT | EQ | GT deriving (Eq, Ord)

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

deriving instance Ord Bool

deriving instance Ord a => Ord [a]

deriving instance Ord ()

deriving instance (Ord a, Ord b) => Ord (a, b)

deriving instance (Ord a, Ord b, Ord c) => Ord (a, b, c)

deriving instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)

