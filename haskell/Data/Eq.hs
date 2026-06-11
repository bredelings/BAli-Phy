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
    
-- Eq Bool
deriving instance Eq Bool
           
-- Eq [a]
deriving instance Eq a => Eq [a]
           
-- Eq ()
deriving instance Eq ()

-- Eq (a,b)
deriving instance (Eq a, Eq b) => Eq (a, b)

-- Eq (a,b,c)
deriving instance (Eq a, Eq b, Eq c) => Eq (a, b, c)

-- Eq (a,b,c,d)
deriving instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
         
