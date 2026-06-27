module Data.Map
    ( Map(..)
    , empty, singleton
    , fromList, fromAscList, fromDescList, fromDistinctAscList, fromDistinctDescList
    , insert, delete, lookup, (!), member, notMember
    , null, size
    , isSubmapOf, isProperSubmapOf
    , filter, map, mapWithKey
    , foldr, foldl
    , elems, keys, assocs, keySet
    , toList, toAscList, toDescList
    ) where

import Prelude hiding (filter,foldl,foldr,null,map,take,drop,splitAt,elems,lookup,(!),empty)
import qualified Data.List as List
import qualified Data.Set as Set

import Data.Eq
import Data.Ord
import Data.Functor
import qualified Data.Foldable as F
import Text.Show

-- The tree is size-balanced.  The cached size supports O(1) size queries and
-- gives insert/delete enough information to preserve logarithmic access.
-- Non-ideal: constructors are exported because the runtime translator needs
-- them for imported function bodies; hide them once abstract exports work.
data Map k a = Tip | Bin Int k a (Map k a) (Map k a)

delta = 3
ratio = 2

empty = Tip

singleton k x = Bin 1 k x Tip Tip

-- Recompute the cached node size after changing children.
bin k x l r = Bin (size l + size r + 1) k x l r

-- Rotate a right-heavy tree one step to the left.
singleL k x l (Bin _ rk rx rl rr) = bin rk rx (bin k x l rl) rr
singleL _ _ _ Tip = error "Data.Map.singleL: Tip"

-- Rotate a right-heavy tree left after first rotating the right child right.
doubleL k x l (Bin _ rk rx (Bin _ rlk rlx rll rlr) rr) =
    bin rlk rlx (bin k x l rll) (bin rk rx rlr rr)
doubleL _ _ _ _ = error "Data.Map.doubleL: invalid tree"

-- Rotate a left-heavy tree one step to the right.
singleR k x (Bin _ lk lx ll lr) r = bin lk lx ll (bin k x lr r)
singleR _ _ Tip _ = error "Data.Map.singleR: Tip"

-- Rotate a left-heavy tree right after first rotating the left child left.
doubleR k x (Bin _ lk lx ll (Bin _ lrk lrx lrl lrr)) r =
    bin lrk lrx (bin lk lx ll lrl) (bin k x lrr r)
doubleR _ _ _ _ = error "Data.Map.doubleR: invalid tree"

-- Restore the size-balance invariant around a possibly changed root.
balance k x l r
    | size l + size r <= 1 = bin k x l r
    | size r > delta * size l = case r of
        Bin _ _ _ rl rr -> if size rl < ratio * size rr then singleL k x l r else doubleL k x l r
        Tip -> error "Data.Map.balance: right Tip"
    | size l > delta * size r = case l of
        Bin _ _ _ ll lr -> if size lr < ratio * size ll then singleR k x l r else doubleR k x l r
        Tip -> error "Data.Map.balance: left Tip"
    | otherwise = bin k x l r

-- Insert by walking the search path and rebalancing nodes on the way back up.
insert k x Tip = singleton k x
insert k x (Bin _ kx x0 l r) =
    case compare k kx of
      LT -> balance kx x0 (insert k x l) r
      GT -> balance kx x0 l (insert k x r)
      EQ -> bin k x l r

-- Remove and return the least key/value pair, preserving balance afterward.
deleteFindMin Tip = error "Data.Map.deleteFindMin: empty map"
deleteFindMin (Bin _ k x Tip r) = ((k, x), r)
deleteFindMin (Bin _ k x l r) =
    let (kx, l') = deleteFindMin l
    in (kx, balance k x l' r)

-- Join two trees whose keys are all less-than/all greater-than each other.
glue Tip r = r
glue l Tip = l
glue l r =
    let ((k, x), r') = deleteFindMin r
    in balance k x l r'

-- Delete by replacing the removed node with the least pair from the right tree.
delete _ Tip = Tip
delete k (Bin _ kx x l r) =
    case compare k kx of
      LT -> balance kx x (delete k l) r
      GT -> balance kx x l (delete k r)
      EQ -> glue l r

-- Find a value by comparing the key against one search path.
lookup _ Tip = Nothing
lookup k (Bin _ kx x l r) =
    case compare k kx of
      LT -> lookup k l
      GT -> lookup k r
      EQ -> Just x

infixl 9 !
(!) m k = case lookup k m of Just x -> x
                             Nothing -> error "Error: element not in the map"

member k m = case lookup k m of Just _ -> True
                                Nothing -> False

notMember k m = not $ member k m

null Tip = True
null _   = False

size Tip = 0
size (Bin s _ _ _ _) = s

-- Build a map by repeated insertion; duplicate keys use the last value.
fromList kxs = List.foldl (\m (k,x) -> insert k x m) empty kxs

-- Compatibility fallback: keep the old API but do not exploit sorted input yet.
-- Replace with a linear builder if construction cost becomes important.
fromAscList kxs = fromList kxs

-- Compatibility fallback: descending input currently takes the general path.
-- Replace with a linear builder if construction cost becomes important.
fromDescList kxs = fromList kxs

-- Build a balanced tree from the first n ascending distinct key/value pairs.
-- The returned tail lets recursive calls share one pass through the input.
buildDistinctAsc 0 kxs = (Tip, kxs)
buildDistinctAsc n kxs =
    let leftN = n `div` 2
        rightN = n - leftN - 1
        (l, (k,x):kxs1) = buildDistinctAsc leftN kxs
        (r, kxs2) = buildDistinctAsc rightN kxs1
    in (bin k x l r, kxs2)

-- Build directly from ascending distinct input; sortedness is assumed, not checked.
fromDistinctAscList kxs = fst (buildDistinctAsc (length kxs) kxs)

-- Compatibility fallback: distinct descending input currently takes the general path.
-- Replace with a linear builder if construction cost becomes important.
fromDistinctDescList kxs = fromList kxs

-- Check whether all key/value pairs from the first map appear in the second map.
isSubmapOf m1 m2 = all present (toAscList m1)
  where
    present (k, x) = case lookup k m2 of
                       Just y -> x == y
                       Nothing -> False

isProperSubmapOf m1 m2 | isSubmapOf m1 m2 = size m1 < size m2
                       | otherwise        = False

-- Filter values in place and use glue to preserve invariants when a root drops.
filter _ Tip = Tip
filter p (Bin _ k x l r) =
    let l' = filter p l
        r' = filter p r
    in if p x then balance k x l' r' else glue l' r'

-- Mapping preserves keys and tree shape, so no key ordering work is needed.
map _ Tip = Tip
map f (Bin s k x l r) = Bin s k (f x) (map f l) (map f r)

-- Mapping with keys preserves keys and tree shape.
mapWithKey _ Tip = Tip
mapWithKey f (Bin s k x l r) = Bin s k (f k x) (mapWithKey f l) (mapWithKey f r)

foldr f z = List.foldr f z . elems

foldl f z = List.foldl f z . elems

elems m = List.map snd $ toAscList m

keys m = List.map fst $ toAscList m

assocs = toAscList

keySet m = Set.fromDistinctAscList $ keys m

toList = toAscList

-- Traverse left-root-right with an accumulator so list construction is linear.
toAscList m = go m []
  where
    go Tip xs = xs
    go (Bin _ k x l r) xs = go l ((k,x):go r xs)

toDescList = reverse . toAscList

instance (Eq k, Eq a) => Eq (Map k a) where
    m1 == m2 = toAscList m1 == toAscList m2

instance (Ord k, Ord a) => Ord (Map k a) where
    compare m1 m2 = compare (toAscList m1) (toAscList m2)

instance (Show k, Show a) => Show (Map k a) where
    show m = "Map " ++ show (toAscList m)

instance Functor (Map k) where
    fmap = map

instance F.Foldable (Map k) where
    toList = elems
