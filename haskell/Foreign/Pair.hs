{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Pair where

data EPair = EPair

builtin c_fst 1 "c_fst" "Pair"
builtin c_snd 1 "c_snd" "Pair"
builtin c_pair 2 "c_pair" "Pair"

pair_from_c p = (c_fst p, c_snd p)

