{-# LANGUAGE NoImplicitPrelude #-}
module Data.Tuple where

fst (x,y) = x
fst3 (x,y,z) = x

snd (x,y) = y
snd3 (x,y,z) = y

thd3 (x,y,z) = z

swap (x,y) = (y,x)

curry f x y = f (x,y)
uncurry f p = f (fst p) (snd p)

