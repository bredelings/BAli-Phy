{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Pair where

data EPair a b = EPair a b

c_fst :: EPair a b -> a
builtin c_fst 1 "Pair:c_fst"

c_snd :: EPair a b -> b
builtin c_snd 1 "Pair:c_snd"

c_pair :: a -> b -> EPair a b
builtin c_pair 2 "Pair:c_pair"

pair_from_c :: EPair a b -> (a,b)
pair_from_c p = (c_fst p, c_snd p)

