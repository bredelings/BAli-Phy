{-# LANGUAGE NoImplicitPrelude #-}
module Data.Function where

id x = x

const x y = x

infixr 9 .
(f . g) x = f (g x)

flip f x y = f y x

infixr 0 $
f $ x = f x

infixl 1 &
x & f = f x

fix f = let x = f x in x

-- i.e. (compare `on` measure) = \x y -> compare (measure x) (measure y)
infixl 0 `on`
on f g = \x y -> g (f x) (f y)
