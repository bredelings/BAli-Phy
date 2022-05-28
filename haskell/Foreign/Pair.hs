{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Pair where

data EPair a b = EPair a b

c_fst :: EPair a b -> a
builtin "Pair:c_fst" c_fst 1

c_snd :: EPair a b -> b
builtin "Pair:c_snd" c_snd 1

c_pair :: a -> b -> EPair a b
builtin "Pair:c_pair" c_pair 2

pair_from_c :: EPair a b -> (a,b)
pair_from_c p = (c_fst p, c_snd p)

