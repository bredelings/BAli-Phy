{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Enum (Bounded(..),
                      Enum(..),
                      enumFrom,
                      enumFromThen,
                      enumFromTo,
                      enumFromThenTo)
    where


import Compiler.Num   -- for -,+
import Compiler.Error -- for error
import Data.Ord       -- for <=
import Data.Bool      -- for otherwise

class Bounded a where
    minBound :: a
    maxBound :: a

instance Bounded Int where
    minBound = -9223372036854775808
    maxBound = 9223372036854775807

instance Bounded Char where
    minBound = intToChar 0
    maxBound = intToChar 0x10ffff

deriving instance Bounded Bool
deriving instance Bounded Ordering
deriving instance Bounded ()

deriving instance (Bounded a, Bounded b) => Bounded (a, b)

deriving instance (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a, b, c, d)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) => Bounded (a, b, c, d, e)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f)
    => Bounded (a, b, c, d, e, f)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g)
    => Bounded (a, b, c, d, e, f, g)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h)
    => Bounded (a, b, c, d, e, f, g, h)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h, Bounded i)
    => Bounded (a, b, c, d, e, f, g, h, i)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h, Bounded i, Bounded j)
    => Bounded (a, b, c, d, e, f, g, h, i, j)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h, Bounded i, Bounded j, Bounded k)
    => Bounded (a, b, c, d, e, f, g, h, i, j, k)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h, Bounded i, Bounded j, Bounded k, Bounded l)
    => Bounded (a, b, c, d, e, f, g, h, i, j, k, l)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m)
    => Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n)
    => Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                   Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o)
    => Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int
    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo   :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

    -- This may wrap instead of throwing an exception on succ maxBound, pred minBound.
    succ x = toEnum (fromEnum x + 1)
    pred x = toEnum (fromEnum x - 1)

    enumFrom n = n:enumFrom (succ n)

    enumFromTo n m | fromEnum n <= fromEnum m     = n : enumFromTo (succ n) m
                   | otherwise                    = []

    enumFromThen n n' = n : n' : worker (f x) (f x n') where
             worker s v = v : worker s (s v)
             x = fromEnum n' - fromEnum n
             f n y  | n > 0 = f (n - 1) (succ y)
                    | n < 0 = f (n + 1) (pred y)
                    | otherwise = y

    enumFromThenTo n n' m = worker (f x) (c x) n m where
             x = fromEnum n' - fromEnum n
             c x = bool (>=) (<=) (x > 0)
             f n y | n > 0      = f (n - 1) (succ y)
                   | n < 0      = f (n + 1) (pred y)
                   | otherwise  = y
             worker s c v m | c (fromEnum v) (fromEnum m)      = v : worker s c (s v) m
                            | otherwise  = []

instance Enum Char where
    toEnum n = intToChar n
    fromEnum n = charToInt n
    succ x = x + 1
    pred x = x - 1

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

instance Enum Int where
    toEnum n = n
    fromEnum n = n
    succ x = x + 1
    pred x = x - 1

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

instance Enum Integer where
    toEnum n = intToInteger n
    fromEnum n = integerToInt n
    succ x = x + 1
    pred x = x - 1

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

instance Enum Double where
    toEnum n = intToDouble n
    fromEnum x = doubleToInt x
    succ x = x + 1
    pred x = x - 1

    enumFromThen from next = enumByFrom (next-from) from

    enumFromThenTo from next to = enumByToFrom (next - from) to from

-- This isn't a standard function -- I made it up..
enumByFrom by from = from:enumByFrom by (from+by)

enumByToFrom by to from | by >= 0 && from <= to  = from:enumByToFrom by to (from+by)
                        | by <  0 && from >= to  = from:enumByToFrom by to (from+by)
                        | otherwise              = []

