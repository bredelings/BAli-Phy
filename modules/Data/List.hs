{-# LANGUAGE NoImplicitPrelude #-}
module Data.List where
{
import Compiler.Base;
import Compiler.Num;
import Data.Bool;
import Data.Maybe;
import Data.Function;

infixr 5 ++;
[] ++ y = y;
h:t ++ y = h:(t ++ y);

head (h:_) = h;
head []    = error "Data.List.head: empty list";

last [x]         =  x;
last (_:xs)      =  last xs;
last []          =  error "Data.List.last: empty list";

tail (_:t) = t;
tail []    = error "Data.List.tail: empty list";

init [x] = [];
init (x:xs) = x:(init xs);
init []     = error "Data.List.init: empty list";

uncons []     = Nothing;
uncons (x:xs) = Just (x,xs);

null []          =  True;
null (_:_)       =  False;

length []        =  0;
length (_:l)     =  1 + length l;

---

map f []  = [];
map f (h:t) = (f h):(map f t);
  
reverse          =  foldl (flip (:)) [];

--intersperse
--intercalate
--transpose
--subsequences
--permutations

---

foldl f z [] = z;
foldl f z (x:xs) = foldl f (f z x) xs;

foldl' f z [] = z;
foldl' f z (x:xs) = let {z' = (f z x)} in seq z' (foldl' f z' xs);

foldr f z [] = z;
foldr f z (x:xs) = (f x (foldr f z xs));

repeat x = xs where {xs = x:xs};

iterate f x = x:iterate f (f x);

replicate n x = take n (repeat x);

take 0 x     = [];
take n []    = [];
take n (h:t) = h:(take (n-1) t);

drop 0 xs     =  xs;
drop _ []     =  [];
drop n (_:xs) =  drop (n-1) xs;

foldl1 f (x:xs)  =  foldl f x xs;
foldl1 _ []      =  error "Data.List.foldl1: empty list";

cycle []         =  error "Data.List.cycle: empty list";
cycle xs         =  let {xs' = xs ++ xs'} in xs';

splitAt n xs  =  (take n xs, drop n xs);


}
