{-# LANGUAGE NoImplicitPrelude #-}
module Data.Tuple where

fst (x,y) = x

snd (x,y) = y

swap (x,y) = (y,x)

curry f x y = f (x,y)
uncurry f p = f (fst p) (snd p)

