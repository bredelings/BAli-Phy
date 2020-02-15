{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Enum (enumFrom,
                      enumFromThen,
                      enumFromTo,
                      enumFromThenTo)
    where

import Data.Ord      -- for <=
import Compiler.Num  -- for -,+
import Data.Bool     -- for otherwise

enumByFrom by from = from:enumByFrom by (from+by)

enumByToFrom by to from | from <= to    = from:enumByToFrom by to (from+by)
                        | otherwise     = []

succ n = n + 1

pred n = n - 1

enumFrom n = n:enumFrom (succ n)

enumFromThen from next = enumByFrom (next-from) from

enumFromThenTo from next to = enumByToFrom (next - from) to from

enumFromTo n m | n <= m    = n:enumFromTo (succ n) m
               | otherwise = []

