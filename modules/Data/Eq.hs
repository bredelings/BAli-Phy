{-# LANGUAGE NoImplicitPrelude #-}
module Data.Eq (module Data.Eq,
                module Data.Bool)
    where

import Data.Bool

builtin builtin_equals 2 "recursive_equals" "Prelude"

infix 4 ==, /=

x      == y      = builtin_equals x y

x /= y = not (x == y)
