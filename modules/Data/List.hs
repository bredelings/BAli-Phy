{-# LANGUAGE NoImplicitPrelude #-}
module Data.List where
{
import Compiler.Base;
import Compiler.Num;
import Data.Bool;
import Data.Maybe;
import Data.Tuple;
import Data.Function;
import Data.Ord;

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

foldl1 f (x:xs)  =  foldl f x xs;
foldl1 _ []      =  error "Data.List.foldl1: empty list";

foldr f z [] = z;
foldr f z (x:xs) = (f x (foldr f z xs));

foldr1 f (x:xs) = foldr x xs;
foldr1 f _      = error "Data.List.foldr1: empty list";

--

concat xs = foldr (++) [] xs;

concatMap f = concat . map f;

and              =  foldr (&&) True;

or               =  foldr (||) False;

any p            =  or . map p;

all p            =  and . map p;

sum     = foldl (+) 0.0;

product = foldl (*) 1.0;

sumi     = foldl (+) 0;

producti = foldl (*) 1;

maximum = foldl1 (max);

minimum = foldl1 (min);

--

-- scanl

-- scanl'

-- scanl1

-- scanr

-- scanr1

--

-- mapAccumL

-- mapAccumR

--

iterate f x = x:iterate f (f x);

iterate' f x = let {fx = f x} in fx `seq` x:iterate f fx;

repeat x = xs where {xs = x:xs};

replicate n x = take n (repeat x);

cycle []         =  error "Data.List.cycle: empty list";
cycle xs         =  let {xs' = xs ++ xs'} in xs';

--

-- unfoldr

--

take 0 x     = [];
take n []    = [];
take n (h:t) = h:(take (n-1) t);

drop 0 xs     =  xs;
drop _ []     =  [];
drop n (_:xs) =  drop (n-1) xs;

splitAt n xs  =  (take n xs, drop n xs);

-- takeWhile

-- dropWhile

-- dropWhileEnd

-- span

-- break

-- stripPrefix

-- group

-- inits

tails (x:xs) = (x:xs):(tails xs);
tails []     = [];

--

-- isPrefixOf

-- isSuffixOf

-- infix 4 `elem`;
-- elem x           =  any (== x);

-- infix 4 `notElem`;
-- notElem x        =  all (/= x);

-- Data.List.lookup
-- lookup key [] = Nothing;
-- lookup key ((k,v):kvs) = if (key == k) then Just v else lookup key kvs;

--

-- find
find _ [] = Nothing;
find p (x:xs) = if (p x) then x else find p xs;

-- FIXME: how to optimize the list comprehension version appropriately?
-- filter p xs = [ x | x <- xs, p x];
filter p [] = [];
filter p (x:xs) = if (p x) then x:(filter p xs) else (filter p xs);                

-- partition

infixr 9 !!;
h:t !! 0 = h;
h:t !! i = t !! (i-1);
_   !! _ = error "Out of bounds list index!";

-- elemIndex

-- elemIndices

-- findIndex

-- findIndices

--

zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs;
zipWith _ _ _           =  [];

zip = zipWith (,);

-- many zips ... 3-7

unzip [] = ([],[]);
unzip [(x,y),l] = ([x:xs],[y:ys]) where {z = unzip l; xs = fst z; ys = snd z};

-- many unzips -- 3-7

-- lines

-- words

-- unlines

-- unwords

-- nub

-- delete

-- (\\)

-- union

-- intersect

-- sort

-- sortOn

-- insert

-- nubBy

-- unionBy

-- intersectBy

-- groupBy

-- sortBy

-- insertBy

-- maximumBy

-- generic*

}
