{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Pair where

data EPair a b

-- These builtins have type variables but refer to unlifted types.

-- In order for these unlifted types to represent C++ objects we currently
--   rely on expression_ref.
    
foreign import ecall "Pair:" c_fst :: EPair a b -> a

foreign import ecall "Pair:" c_snd :: EPair a b -> b

foreign import ecall "Pair:" c_pair :: a -> b -> EPair a b

c_pair' (x,y) = c_pair x y

pair_from_c :: EPair a b -> (a,b)
pair_from_c p = (c_fst p, c_snd p)

