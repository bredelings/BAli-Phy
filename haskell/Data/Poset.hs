{-# LANGUAGE NoImplicitPrelude #-}
module Data.Poset where
-- This also provides Ord, but with a different backing.

import Data.Eq

data Ordering = LT | EQ | GT | NC

infix 4 <, <=, >, >=
infix 4 <==>, </=> -- I presume these are the same...

class Eq a => Poset a where

    compare :: a -> a -> Ordering

    -- comparable
    (<==>) :: a -> a -> Bool

    -- not comparable
    (</=>) :: a -> a -> Bool

    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (>) :: a -> a -> Bool

    compare x y | x < y     = LT
                | x == y    = EQ
                | x > y     = GT
                | otherwise = NC

    x </=> y = not (x <==> y)
    x <==> y = not (x </=> y)

    x < y = x<==>y && not (x >= y)
    x > y = x<==>y && not (x <= y)
    x >= y = x > y || x == y
    x <= y = x < y || x == y

-- Ignore NaNs
class Poset a => Sortable a where
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    isOrdered :: a -> Bool
    max :: a -> a -> a
    min :: a -> a -> a

-- Alternative Ord implementation that does not include Double
class Sortable a => Ord a

-- sort :: Sortable a => [a] -> [a]

-- comparing :: Poset b => (a -> b) -> a -> a -> Ordering

foreign import ecall "Prelude:" lessthan_char :: Char -> Char -> Bool
foreign import ecall "Prelude:" lessthan_int :: Int -> Int -> Bool
foreign import ecall "Prelude:" lessthan_integer :: Integer -> Integer -> Bool
foreign import ecall "Prelude:" lessthan_double :: Double -> Double -> Bool

instance Poset Char where
    x <==> y = True

    (<) = lessthan_char

instance Poset Int where
    x <==> y = True

    (<) = lessthan_int

instance Poset Integer where
    x <==> y = True

    (<) = lessthan_integer

instance Poset Double where
    x <==> y = True

    (<) = lessthan_double

