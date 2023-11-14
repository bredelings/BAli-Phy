{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Pair where

data EPair a b

foreign import bpcall "Pair:c_fst" c_fst :: EPair a b -> a

foreign import bpcall "Pair:c_snd" c_snd :: EPair a b -> b

foreign import bpcall "Pair:c_pair" c_pair :: a -> b -> EPair a b

c_pair' (x,y) = c_pair x y

pair_from_c :: EPair a b -> (a,b)
pair_from_c p = (c_fst p, c_snd p)

