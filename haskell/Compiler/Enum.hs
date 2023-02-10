{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Enum (Enum,
                      enumFrom,
                      enumFromThen,
                      enumFromTo,
                      enumFromThenTo)
    where


import Compiler.Num   -- for -,+
import Compiler.Error -- for error
import Data.Ord       -- for <=
import Data.Bool      -- for otherwise

class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int
    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo   :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

    enumFrom n = n:enumFrom (succ n)


instance Enum Char where
    succ n = n + 1
    pred n = n - 1
    toEnum n = intToChar n
    fromEnum n = charToInt n

    enumFromTo n m | fromEnum n <= fromEnum m    = n:enumFromTo (succ n) m
                   | otherwise                   = []

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

instance Enum Int where
    succ n = n + 1
    pred n = n - 1
    toEnum n = n
    fromEnum n = n

    enumFromTo n m | fromEnum n <= fromEnum m    = n:enumFromTo (succ n) m
                   | otherwise                   = []

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

instance Enum Integer where
    succ n = n + 1
    pred n = n - 1
    toEnum n = intToInteger n
    fromEnum n = integerToInt n

    enumFromTo n m | fromEnum n <= fromEnum m    = n:enumFromTo (succ n) m
                   | otherwise                   = []

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

instance Enum Double where
    succ x = x + 1
    pred x = x - 1
    toEnum n = intToDouble n
    fromEnum x = error "fromEnum: Double"

    enumFromTo x y | x < y      = x:enumFromTo (x+1) y
                   | otherwise  = []

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

-- This isn't a standard function -- I made it up..
enumByFrom by from = from:enumByFrom by (from+by)

-- This isn't right for negative "by"
enumByToFrom by to from | from <= to    = from:enumByToFrom by to (from+by)
                        | otherwise     = []



