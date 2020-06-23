module Data.Map where

import Prelude hiding (filter,foldl,foldr,null,map,take,drop,splitAt,elems,lookup,(!))
import qualified Data.List as List
import qualified Data.Set as Set

data Map k a = Map [(k,a)]

empty = Map []

singleton k x = Map [(k,x)]

fromList kxs = Map $ nubBy (\kx1 kx2 -> fst kx1 == fst kx2) kxs

fromAscList kxs = fromList kxs

fromDescList kxs = fromList kxs

fromDistinctAscList kxs = fromList kxs

fromDistinctDescList kxs = fromList kxs



insert k x m = (k,x):(delete k m)

delete k m@(Map xs) = Map $ List.filter (\kx -> fst kx /= k) xs

member k m = List.elem k $ toList m

notMember k m = not $ member k m


null (Map []) = True
null _        = False

size (Map kxs) = length kxs

isSubmapOf m1 m2 = Set.isSubsetOf (keySet m1) (keySet m2)

isProperSubmapOf m1 m2 = Set.isProperSubsetOf (keySet m1) (keySet m2)

--disjoint s1 s2 = size (intersection s1 s2) == 0

--union (Set xs) (Set ys) = Set (xs ++ List.filter (`notElem` xs) ys)

--unions = List.foldl union empty

--difference (Set xs) (Set ys) = Set $ List.filter (`notElem` ys) xs

-- infixl 9 \\
-- s1 \\ s2 = s1 difference s2

--intersection (Set xs) (Set ys) = Set $ List.filter (`elem` ys) xs

--cartesionProduct (Set xs) (Set ys) = Set [(x,y) | x <- xs, y <- ys]

--disjointUnion xs ys = map Left xs `union` map Right ys

filter pred xs = Set.Set $ List.filter (\(_,x) -> pred x) xs

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

map f (Map xs) = Map $ map (\(k,x) -> (k,f x)) xs

mapWithKey f (Map xs) = Map $ map (\(k,x) -> (k,f k x)) xs

-- mapMonotonic

foldr f z = foldr f z . elems

foldl f z = foldl f z . elems

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

lookup k' (Map kxs) = go kxs where
    go [] = Nothing
    go ((k,x):kxs) | k' == k   = Just x
                   | otherwise = go kxs

infixl 9 !

m ! k = case lookup k m of Just x -> x
                           Nothing -> "Error: element not in the map"

m ! k = case lookup k m of Just x -> x
                           Nothing -> "Error: element not in the map"

elems m = List.map snd $ toAscList m

keys m = List.map fst $ toAscList m

assocs = toAscList

keySet m = Set.fromList $ keys m



toList (Map xs) = xs

toAscList (Map xs) = List.sortOn snd xs

toDescList = reverse . toAscList

