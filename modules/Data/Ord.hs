{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ord (module Data.Ord,
                 module Data.Eq)
where

import Data.Eq

data Ordering = EQ | LT | GT

builtin builtin_compare 2 "recursive_compare" "Prelude"

infix 4 <, <=, >, >=

compare x y = case builtin_compare x y of 0    -> EQ
                                          1    -> GT
                                          _    -> LT

x < y = case compare x y of LT -> True
                            _  -> False

x <= y = case compare x y of GT -> False
                             _  -> True

x > y = case compare x y of GT -> True
                            _  -> False

x >= y = case compare x y of LT -> False
                             _  -> True

min x y = if (x <= y) then x else y
max x y = if (x >= y) then x else y
