{-# LANGUAGE NoImplicitPrelude #-}
module Data.Eq (module Data.Bool,
                (==),
                (/=)
               )
    where

import Data.Bool
import Compiler.Num          -- for (-)
import Foreign.Introspection -- for get_arg, get_n_args

builtin equals_top 2 "equals_top" "Prelude"

class Eq a where {}
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--    x == y = not (x /= y)
--    x /= y = not (x == y)

instance Eq Int where
    x == y = not (x /= y)


infix 4 ==, /=

and []      = True
and (x:xs)  = x && (and xs)

x == y      = case equals_top x y of 1 -> False
                                     2 -> True
                                     _ -> and [get_arg x i == get_arg y i | i <- [0..get_n_args x - 1]]

x /= y = not (x == y)

instance Eq a => Eq [a] where
    []     == []     = True
    (x:xs) == (y:ys) = (x == y) && xs == ys
    _      == _      = False

class (Eq a, Eq [a]) => Foo a where
    infix 6 `fffff`
    fffff :: a -> a -> a
    fffff x y = y

