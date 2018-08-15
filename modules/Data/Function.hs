{-# LANGUAGE NoImplicitPrelude #-}
module Data.Function where
{
id x = x;

flip f x y = f y x;

const x y = x;

infixr 0 $;
f $ x = f x;

infixl 1 &;
x & f = f x;
}
