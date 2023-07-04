module Data.IntSet where

import Prelude hiding (map,empty,elems)
import Data.Functor
import Foreign.Vector
import Data.Foldable (foldr)

data IntSet

type Key = Int

foreign import bpcall "IntSet:empty" _empty :: () -> IntSet
empty = _empty ()

foreign import bpcall "IntSet:" singleton :: Key -> IntSet

-- Should this be more efficient? With immer it might be OK, but maybe we should
-- convert to an EVector Int, and then convert that to an IntSet
fromList []     = empty
fromList (k:ks) = insert k $ fromList ks

foreign import bpcall "IntSet:" insert :: Key -> IntSet -> IntSet

foreign import bpcall "IntSet:" delete :: Key -> IntSet -> IntSet

foreign import bpcall "IntSet:" member :: Int -> IntSet -> Bool

notMember k = not . member k

-- lookupLT
-- lookupGT
-- lookupLE
-- lookup GE

null m = size m == 0

foreign import bpcall "IntSet:" size :: IntSet -> Int

foreign import bpcall "IntSet:" union :: IntSet -> IntSet -> IntSet

unions :: Foldable f => f (IntSet) -> IntSet
unions sets = foldr union empty sets

foreign import bpcall "IntSet:" difference :: IntSet -> IntSet -> IntSet

(\\) = difference

-- isSubsetOf :: IntSet -> IntSet -> Bool

-- isProperSubsetOf :: IntSet -> IntSet -> Bool 

foreign import bpcall "IntSet:" intersection :: IntSet -> IntSet -> IntSet

foreign import bpcall "IntSet:" disjoint :: IntSet -> IntSet -> Int

-- Note!  These are supposed be to in ascending order of keys, but are not.

foreign import bpcall "IntSet:keys" _keys :: IntSet -> EVector Key
elems m = vector_to_list $ _keys m

toList m = elems m

toAscList m = toList m

toDescList m = toList m

-- filter :: (Key -> bool) -> IntSet -> IntSet
-- partition :: (Key -> Bool) -> IntSet -> (IntSet, IntSet)
-- split :: Key -> IntSet -> (IntSet, IntSet)
-- splitMember :: Key -> IntSet -> (IntSet, Bool, IntSet)
-- splitRoot :: IntSet -> [IntSet]

map :: (Key -> Key) -> IntSet -> IntSet
map f set = fromList [ f key | key <- toList set]
-- mapMonotonic :: (Key -> Key) -> IntSet -> IntSet

-- isSubmapOf :: Eq a => IntSet -> IntSet -> Bool
-- isSubmapOfBy :: (a -> b -> Bool) -> IntSet -> IntSet b -> Bool
-- isProperSubmapOf :: Eq a => IntSet -> IntSet -> Bool
-- isProperSubmapOfBy :: (a -> b -> Bool) -> IntSet -> IntSet b -> Bool

-- findMin :: IntSet -> Key
-- findMax :: IntSet -> Key
-- deleteMin :: IntSet -> IntSet
-- deleteMax :: IntSet -> IntSet
-- deleteFindMin :: IntSet -> (Key, IntSet)
-- deleteFindMax :: IntSet -> (Key, IntSet)
-- maxView :: IntSet -> Maybe (Key, IntSet)
-- minView :: IntSet -> Maybe (Key, IntSet) 

instance Show (IntSet) where
    show m = show $ toList m
