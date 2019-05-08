{-# LANGUAGE NoImplicitPrelude #-}
module Data.List where

import Compiler.Base
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Tuple
import Data.Function
import Data.Ord

infixr 5 ++
[] ++ y = y
(h:t) ++ y = h:(t ++ y)

head (h:_) = h
head []    = error "Data.List.head: empty list"

last [x]         =  x
last (_:xs)      =  last xs
last []          =  error "Data.List.last: empty list"

tail (_:t) = t
tail []    = error "Data.List.tail: empty list"

init [x] = []
init (x:xs) = x:(init xs)
init []     = error "Data.List.init: empty list"

uncons []     = Nothing
uncons (x:xs) = Just (x,xs)

null []          =  True
null (_:_)       =  False

length []        =  0
length (_:l)     =  1 + length l

---

map f []  = []
map f (h:t) = (f h):(map f t)
  
reverse          =  foldl (flip (:)) []

--

intersperse _   []      = []
intersperse _   [x]     = [x]
intersperse sep (x1:x2:xs) = x1:sep:(intersperse sep (x2:xs))

intercalate xs xss = concat (intersperse xs xss)

--transpose

subsequences [] = [[]]
subsequences (x:xs) = let ys = subsequences xs in xs ++ (map (x:) xs)

permutations [] = [[]]
permutations (x:xs) = let ys = permutations xs in map (x:) ys ++ map (++[x]) ys

---

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl' f z [] = z
foldl' f z (x:xs) = let z' = (f z x) in seq z' (foldl' f z' xs)

foldl1 f (x:xs)  =  foldl f x xs
foldl1 _ []      =  error "Data.List.foldl1: empty list"

foldr f z [] = z
foldr f z (x:xs) = (f x (foldr f z xs))

foldr1 _ [x]    = [x]
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 f _      = error "Data.List.foldr1: empty list"

--

concat xs = foldr (++) [] xs

concatMap f = concat . map f

and              =  foldr (&&) True

or               =  foldr (||) False

any p            =  or . map p

all p            =  and . map p

sum     = foldl (+) 0.0

product = foldl (*) 1.0

sumi     = foldl (+) 0

producti = foldl (*) 1

maximum = foldl1 (max)

minimum = foldl1 (min)

--

scanl f z []      = [z]
scanl f z (x:xs)  = let zx = z `f` x in (zx:scanl f zx xs)

scanl' f z []     = [z]
scanl' f z (x:xs) = let zx = z `f` x in zx `seq` (zx:scanl f zx xs)

scanl1 f (x:xs)   = scanl f x xs

scanr  f z []     = [z]
scanr  f z (x:xs) = let ys = scanr f z xs in (x `f` (head ys)):ys

scanr1 f [x]    = [x]
scanr1 f (x:xs) = let ys = scanr1 f xs in (x `f` (head ys)):ys
scanr1 _ []     = error "Data.List.scanr1: empty list"

--

-- mapAccumL

-- mapAccumR

--

iterate f x = x:iterate f (f x)

iterate' f x = let fx = f x in fx `seq` x:iterate f fx

repeat x = xs where xs = x:xs

replicate n x = take n (repeat x)

cycle []         =  error "Data.List.cycle: empty list"
cycle xs         =  let xs' = xs ++ xs' in xs'

--

-- unfoldr

--

take 0 x     = []
take n []    = []
take n (h:t) = h:(take (n-1) t)

drop 0 xs     =  xs
drop _ []     =  []
drop n (_:xs) =  drop (n-1) xs

splitAt n xs  =  (take n xs, drop n xs)

-- takeWhile

-- dropWhile

-- dropWhileEnd

-- span

-- break

-- stripPrefix

-- group

-- inits

tails (x:xs) = (x:xs):(tails xs)
tails []     = []

--

-- isPrefixOf

-- isSuffixOf

infix 4 `elem`
elem x           =  any (== x)

infix 4 `notElem`
notElem x        =  all (/= x)

lookup key [] = Nothing
lookup key ((k,v):kvs) = if (key == k) then Just v else lookup key kvs

--

-- find
find p = listToMaybe . filter p

-- FIXME: how to optimize the list comprehension version appropriately?
-- filter p xs = [ x | x <- xs, p x]
filter p [] = []
filter p (x:xs) = if (p x) then x:(filter p xs) else (filter p xs)                

-- partition

infixr 9 !!
(h:t) !! 0 = h
(h:t) !! i = t !! (i-1)
_     !! _ = error "Out of bounds list index!"

-- elemIndex
elemIndex key  = listToMaybe . elemIndices key

-- elemIndices
elemIndices key = findIndices (key==)

-- findIndex
findIndex p = listToMaybe . findIndices p

-- findIndices
findIndices p xs = [i | (i,x) <- zip [0..] xs, p x]
--

zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith _ _ _           =  []

zip = zipWith (,)

-- many zips ... 3-7

unzip [] = ([],[])
unzip [(x,y),l] = ([x:xs],[y:ys]) where z = unzip l
                                        xs = fst z
                                        ys = snd z

-- many unzips -- 3-7

-- lines

-- words

-- unlines

-- unwords

-- nub
nub = nubBy (==)

-- delete

-- (\\)

-- union

-- intersect

-- sort
sort  = sortOn id

-- sortOn
sortOn f [] = []
sortOn f (x:xs) = sortOn f small ++ (x : sortOn f large)
   where small = [y | y <- xs, (f y) <= (f x)]
         large = [y | y <- xs, (f y)  > (f x)]

-- insert

--nubBy
nubBy eq (x:xs) = x:nubBy eq (filter (\y -> not (eq x y)) xs)
nubBy eq [] = []


-- unionBy

-- intersectBy

-- groupBy

-- sortBy

-- insertBy

-- maximumBy

-- generic*


