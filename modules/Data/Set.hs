module Data.Set where

import Prelude hiding (filter,foldl,foldr,null,map,take,drop,splitAt)
import qualified Data.List as List
    
-- This should at the minimum allow adding and removing elements, and
-- holding identical elements at most once.

-- I'm not sure I can effectively use Ord without type classes.
-- Could maybe implement a builtin_compare function that handles
--   Int, Double, LogDouble, Char, String, List, Tuples

data Set a = Set [a]

empty = Set []

singleton x = Set [x]

fromList xs = Set (nub xs)

fromAscList xs = fromList xs

fromDescList xs = fromList xs

fromDistinctAscList xs = fromList xs

fromDistinctDescList xs = fromList xs

powerSet (Set [])  = Set [[]]
powerSet (Set (x:xs)) = let Set ys = powerSet xs
                        in Set (ys++(List.map (x:) ys))

insert x s@(Set xs) | member x s  = s
                    | otherwise   = Set (x:xs)

delete x s@(Set xs) = Set $ List.filter (/=x) xs

member x s@(Set xs) | elem x xs  = True
                    | otherwise  = False

notMember x s = not $ member x s

-- lookupLT

-- lookupGT

-- lookupLE

-- lookupGE

null (Set []) = True
null _        = False

size (Set xs) = length xs

isSubsetOf (Set []) s2 = True

isSubsetOf (Set xs) (Set ys) = go xs ys where
    go []     ys                = True
    go (x:xs) ys | x `elem` ys  = go xs ys
                 | otherwise    = False

isProperSubsetOf s1 s2 | isSubsetOf s1 s2 = size s1 < size s2
                       | otherwise        =  False

disjoint s1 s2 = size (intersection s1 s2) == 0

union (Set xs) (Set ys) = Set (xs ++ List.filter (`notElem` xs) ys)

unions = List.foldl union empty

difference (Set xs) (Set ys) = Set $ List.filter (`notElem` ys) xs

infixl 9 \\
s1 \\ s2 = s1 difference s2

intersection (Set xs) (Set ys) = Set $ List.filter (`elem` ys) xs

cartesionProduct (Set xs) (Set ys) = Set [(x,y) | x <- xs, y <- ys]

disjointUnion xs ys = map Left xs `union` map Right ys

filter pred xs = Set $ List.filter pred xs

-- takeWhileAntitone

-- dropWhileAntitone

-- spanAntitone

-- partition

-- split

-- splitMember

-- splitRoot

-- lookupIndex

-- findIndex

-- lookupIndex

-- findIndex

-- elemAt

-- deleteAt

-- take n (Set xs) = 

-- drop

-- splitAt

map f (Set xs) = Set $ map f xs

-- mapMonotonic

-- foldr

-- foldl

--foldr'

--foldl'

--fold

--lookupMin

--lookupMax

--findMin

--findMax

--deleteMin

--deleteMax

--deleteFindMin

--deleteFindMax

--maxView

-- minView

elems = toAscList

toList (Set xs) = xs

toAscList (Set xs) = List.sort xs

toDescList = reverse . toAscList

