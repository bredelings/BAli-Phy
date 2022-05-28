{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Introspection where

get_arg :: a -> Int -> b
foreign import bpcall "Prelude:get_arg" get_arg    :: () -> () -> ()

get_n_args :: a -> Int
foreign import bpcall "Prelude:get_n_args" get_n_args :: () -> ()

-- foreign import bpcall1 "Prelude:get_arg" get_arg :: a -> Int -> b

-- foreign import bpcall1 "Prelude:get_n_args" get_n_args :: a -> Int
