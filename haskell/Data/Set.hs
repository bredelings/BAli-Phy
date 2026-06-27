module Data.Set where

import Prelude hiding (filter,foldl,foldr,null,map,take,drop,splitAt,empty)
import qualified Data.List as List
import qualified Data.Foldable as F

-- The tree is size-balanced.  The cached size supports O(1) size queries and
-- gives the balancing code enough information to keep operations logarithmic.
data Set a = Tip | Bin Int a (Set a) (Set a)

delta = 3
ratio = 2

empty = Tip

singleton x = Bin 1 x Tip Tip

-- Recompute the cached node size after changing children.
bin x l r = Bin (size l + size r + 1) x l r

-- Rotate a right-heavy tree one step to the left.
singleL x l (Bin _ y rl rr) = bin y (bin x l rl) rr
singleL _ _ Tip = error "Data.Set.singleL: Tip"

-- Rotate a right-heavy tree left after first rotating the right child right.
doubleL x l (Bin _ z (Bin _ y rll rlr) rr) = bin y (bin x l rll) (bin z rlr rr)
doubleL _ _ _ = error "Data.Set.doubleL: invalid tree"

-- Rotate a left-heavy tree one step to the right.
singleR x (Bin _ y ll lr) r = bin y ll (bin x lr r)
singleR _ Tip _ = error "Data.Set.singleR: Tip"

-- Rotate a left-heavy tree right after first rotating the left child left.
doubleR x (Bin _ z ll (Bin _ y lrl lrr)) r = bin y (bin z ll lrl) (bin x lrr r)
doubleR _ _ _ = error "Data.Set.doubleR: invalid tree"

-- Restore the size-balance invariant around a possibly changed root.
balance x l r
    | size l + size r <= 1 = bin x l r
    | size r > delta * size l = case r of
        Bin _ _ rl rr -> if size rl < ratio * size rr then singleL x l r else doubleL x l r
        Tip -> error "Data.Set.balance: right Tip"
    | size l > delta * size r = case l of
        Bin _ _ ll lr -> if size lr < ratio * size ll then singleR x l r else doubleR x l r
        Tip -> error "Data.Set.balance: left Tip"
    | otherwise = bin x l r

-- Insert by walking the search path and rebalancing nodes on the way back up.
insert x Tip = singleton x
insert x (Bin _ y l r) =
    case compare x y of
      LT -> balance y (insert x l) r
      GT -> balance y l (insert x r)
      EQ -> bin x l r

-- Remove and return the least element, preserving balance in the remaining tree.
deleteFindMin Tip = error "Data.Set.deleteFindMin: empty set"
deleteFindMin (Bin _ x Tip r) = (x, r)
deleteFindMin (Bin _ x l r) =
    let (xm, l') = deleteFindMin l
    in (xm, balance x l' r)

-- Join two trees whose elements are all less-than/all greater-than each other.
glue Tip r = r
glue l Tip = l
glue l r =
    let (x, r') = deleteFindMin r
    in balance x l r'

-- Delete by replacing the removed node with the least element from the right tree.
delete _ Tip = Tip
delete x (Bin _ y l r) =
    case compare x y of
      LT -> balance y (delete x l) r
      GT -> balance y l (delete x r)
      EQ -> glue l r

-- Find an element by comparing against each node on one search path.
member _ Tip = False
member x (Bin _ y l r) =
    case compare x y of
      LT -> member x l
      GT -> member x r
      EQ -> True

notMember x s = not $ member x s

null Tip = True
null _   = False

size Tip = 0
size (Bin s _ _ _) = s

-- Build a set by repeated insertion; duplicate elements collapse naturally.
fromList xs = List.foldl (\s x -> insert x s) empty xs

-- Compatibility fallback: keep the old API but do not exploit sorted input yet.
-- Replace with a linear builder if construction cost becomes important.
fromAscList xs = fromList xs

-- Compatibility fallback: descending input currently takes the general path.
-- Replace with a linear builder if construction cost becomes important.
fromDescList xs = fromList xs

-- Compatibility fallback: distinct ascending input currently takes the general path.
-- Replace with a linear builder if construction cost becomes important.
fromDistinctAscList xs = fromList xs

-- Compatibility fallback: distinct descending input currently takes the general path.
-- Replace with a linear builder if construction cost becomes important.
fromDistinctDescList xs = fromList xs

-- Build the set of all subsets from the ordered element list.
powerSet s = fromList (go (toList s))
  where
    go [] = [[]]
    go (x:xs) = let ys = go xs in ys ++ List.map (x:) ys

-- Test subset inclusion by checking every element from the smaller API surface.
isSubsetOf s1 s2 = all (\x -> member x s2) (toList s1)

isProperSubsetOf s1 s2 | isSubsetOf s1 s2 = size s1 < size s2
                       | otherwise        = False

disjoint s1 s2 = size (intersection s1 s2) == 0

-- Merge by inserting all elements from the first set into the second set.
union s1 s2 = List.foldl (\s x -> insert x s) s2 (toList s1)

unions = List.foldl union empty

-- Remove each element in the second set from the first set.
difference s1 s2 = List.foldl (\s x -> delete x s) s1 (toList s2)

infixl 9 \\
s1 \\ s2 = s1 `difference` s2

-- Keep elements from the first set only when they are also in the second set.
intersection s1 s2 = fromList [x | x <- toList s1, member x s2]

-- Produce the Cartesian product while preserving set uniqueness.
cartesionProduct s1 s2 = fromList [(x,y) | x <- toList s1, y <- toList s2]

disjointUnion xs ys = map Left xs `union` map Right ys

-- Filter in place and use glue to preserve tree invariants when a root is dropped.
filter _ Tip = Tip
filter p (Bin _ x l r) =
    let l' = filter p l
        r' = filter p r
    in if p x then balance x l' r' else glue l' r'

-- Mapping may change element order, so rebuild through fromList.
map f s = fromList $ List.map f $ toList s

elems = toAscList

toList = toAscList

-- Traverse left-root-right to expose the ordered representation.
toAscList Tip = []
toAscList (Bin _ x l r) = toAscList l ++ (x:toAscList r)

toDescList = reverse . toAscList

instance Eq a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Ord a => Ord (Set a) where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)

instance Show a => Show (Set a) where
    show xs = "Set " ++ show (toAscList xs)

instance F.Foldable Set where
    toList = toList
