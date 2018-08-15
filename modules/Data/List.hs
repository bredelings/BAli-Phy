{-# LANGUAGE NoImplicitPrelude #-}
module Data.List where
{
import Compiler.Num;
import Data.Bool;
foldr f z [] = z;
foldr f z (x:xs) = (f x (foldr f z xs));

foldl f z [] = z;
foldl f z (x:xs) = foldl f (f z x) xs;

null []          =  True;
null (_:_)       =  False;

length []        =  0;
length (_:l)     =  1 + length l;

repeat x = xs where {xs = x:xs};

iterate f x = x:iterate f (f x);

replicate n x = take n (repeat x);

take 0 x     = [];
take n []    = [];
take n (h:t) = h:(take (n-1) t);

drop 0 xs     =  xs;
drop _ []     =  [];
drop n (_:xs) =  drop (n-1) xs;

infixr 5 ++;
[] ++ y = y;
h:t ++ y = h:(t ++ y);

map f []  = [];
map f (h:t) = (f h):(map f t);
  
}
