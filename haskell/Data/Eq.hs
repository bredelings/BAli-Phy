{-# LANGUAGE NoImplicitPrelude #-}
module Data.Eq (module Data.Bool,
                Eq,
                (==),
                (/=)
               )
    where

import Data.Bool

infix 4 ==, /=

class Eq a where
    (==) :: a -> a -> Bool

x /= y = not (x == y)

-- Eq Char
foreign import ecall "Prelude:" equals_char :: Char -> Char -> Bool

instance Eq Char where
    (==) = equals_char

-- Eq Int
foreign import ecall "Prelude:" equals_int :: Int -> Int -> Bool

instance Eq Int where
    (==) = equals_int

-- Eq Integer
foreign import ecall "Prelude:" equals_integer :: Integer -> Integer-> Bool

instance Eq Integer where
    (==) = equals_integer

-- Eq Double
foreign import ecall "Prelude:" equals_double :: Double -> Double -> Bool

instance Eq Double where
    (==) = equals_double
    
-- Eq [a]
instance Eq a => Eq [a] where
    []     == []     = True
    (x:xs) == (y:ys) = (x == y) && (xs == ys)
    _      == _      = False
           
-- Eq (a,b)
instance (Eq a, Eq b) => Eq (a,b) where
    (x1,y1) == (x2,y2) = (x1 == x2) && (y1 == y2)

-- Eq (a,b,c)
instance (Eq a, Eq b, Eq c) => Eq (a, b ,c) where
    (x1,y1,z1) == (x2,y2,z2) = (x1 == x2) && (y1 == y2) && (z1 == z2)
