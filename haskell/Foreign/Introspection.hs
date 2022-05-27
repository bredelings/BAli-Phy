{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Introspection where

get_arg :: a -> Int -> b
builtin get_arg    2 "Prelude:get_arg"

get_n_args :: a -> Int
builtin get_n_args 1 "Prelude:get_n_args"

-- foreign import bpcall1 "Prelude:get_arg" get_arg :: a -> Int -> b

-- foreign import bpcall1 "Prelude:get_n_args" get_n_args :: a -> Int
