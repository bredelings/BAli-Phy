{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Introspection where

foreign import bpcall "Prelude:get_arg" get_arg    :: a -> Int -> b

foreign import bpcall "Prelude:get_n_args" get_n_args :: a -> Int
