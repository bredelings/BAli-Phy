{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ord (module Data.Eq,
                (<),
                (<=),
                (>),
                (>=),
                compare,
                min,
                max)
where

import Data.Eq
import Compiler.Num          -- for (-)
import Foreign.Introspection -- for get_arg, get_n_args

data Ordering = EQ | LT | GT

builtin compare_top 2 "compare_top" "Prelude"

infix 4 <, <=, >, >=

EQ <&> y = y
x  <&> _ = x

compare_all []     = EQ
compare_all (x:xs) = x <&> (compare_all xs)

compare x y      = case compare_top x y of 4 -> LT
                                           5 -> GT
                                           6 -> EQ
                                           _ -> compare_all [get_arg x i `compare` get_arg y i | i <- [0..get_n_args x - 1]]

x < y  = case compare x y of LT -> True
                             _  -> False

x <= y = case compare x y of GT -> False
                             _  -> True

x > y  = case compare x y of GT -> True
                             _  -> False

x >= y = case compare x y of LT -> False
                             _  -> True

min x y = if (x <= y) then x else y
max x y = if (x >= y) then x else y
