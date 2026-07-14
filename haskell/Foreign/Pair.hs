{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Pair
    ( EPair
    , c_fst
    , c_snd
    , c_pair
    , c_pair'
    , pair_from_c
    ) where

import Compiler.FFI.Runtime (RuntimeValue)

data EPair a b

instance RuntimeValue (EPair a b)

-- These builtins have type variables but refer to unlifted types.

-- These unlifted types represent C++ objects through Runtime::Exp values.
    
foreign import ecall "Pair:" c_fst :: EPair a b -> a

foreign import ecall "Pair:" c_snd :: EPair a b -> b

foreign import ecall "Pair:c_pair"
    c_pair_raw :: a -> b -> EPair a b

c_pair :: (RuntimeValue a, RuntimeValue b) => a -> b -> EPair a b
c_pair = c_pair_raw

c_pair' :: (RuntimeValue a, RuntimeValue b) => (a,b) -> EPair a b
c_pair' (x,y) = c_pair x y

pair_from_c :: EPair a b -> (a,b)
pair_from_c p = (c_fst p, c_snd p)
