module Data.IntSet where

import Prelude hiding (map,empty,elems)
import Data.Functor
import Foreign.Vector

data IntSet

type Key = Int

foreign import bpcall "IntSet:empty" _empty :: () -> IntSet
empty = _empty ()

foreign import bpcall "IntSet:" singleton :: Key -> IntSet

-- fromSet :: (Key -> a) -> IntSet -> IntSet

fromList []     = empty
fromList (k:ks) = insert k $ fromList ks

foreign import bpcall "IntSet:insert" insert :: Key -> IntSet -> IntSet

foreign import bpcall "IntSet:delete" delete :: Key -> IntSet -> IntSet

foreign import bpcall "IntSet:has_key" builtin_member :: Int -> IntSet -> Int
member key m = case builtin_member key m of 1 -> True ; _ -> False

notMember k = not . member k

-- lookupLT
-- lookupGT
-- lookupLE
-- lookup GE

null m = size m == 0

foreign import bpcall "IntSet:" size :: IntSet -> Int

foreign import bpcall "IntSet:" union :: IntSet -> IntSet -> IntSet

-- unions :: Foldable f => f (IntSet) -> IntSet

foreign import bpcall "IntSet:" difference :: IntSet -> IntSet -> IntSet

(\\) = difference

foreign import bpcall "IntSet:" intersection :: IntSet -> IntSet -> IntSet

foreign import bpcall "IntSet:disjoint" _disjoint :: IntSet -> IntSet -> Int
disjoint m1 m2 = case _disjoint m1 m2 of
                   0 -> False
                   _ -> True

-- Note!  These are supposed be to in ascending order of keys, but are not.

foreign import bpcall "IntSet:keys" _keys :: IntSet -> EVector Key
elems m = vector_to_list $ _keys m

toList m = elems m

toAscList m = toList m

toDescList m = toList m

-- filter :: (a -> bool) -> IntSet -> IntSet
-- filterWithKey :: (Key -> a -> Bool) -> IntSet -> IntSet
-- restrictKeys :: IntSet -> IntSet -> IntSet
-- withoutKeys :: IntSet -> IntSet -> IntSet
-- partition :: (a -> Bool) -> IntSet -> (IntSet, IntSet)
-- partitionWithKey :: (Key -> a -> Bool) -> IntSet -> (IntSet, IntSet)
-- mapMaybe :: (a -> Maybe b) -> IntSet -> IntSet b
-- mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntSet -> IntSet b
-- mapEither :: (a -> Either b c) -> IntSet -> (IntSet b, IntSet c)
-- mapEitherWithKey :: (Key -> a -> Either b c) -> IntSet -> (IntSet b, IntSet c)
-- split :: Key -> IntSet -> (IntSet, IntSet)
-- splitLookup :: Key -> IntSet -> (IntSet, Maybe a, IntSet)
-- splitRoot :: IntSet -> [IntSet]

-- isSubmapOf :: Eq a => IntSet -> IntSet -> Bool
-- isSubmapOfBy :: (a -> b -> Bool) -> IntSet -> IntSet b -> Bool
-- isProperSubmapOf :: Eq a => IntSet -> IntSet -> Bool
-- isProperSubmapOfBy :: (a -> b -> Bool) -> IntSet -> IntSet b -> Bool

-- lookupMin :: IntSet -> Maybe (Key, a)
-- lookupMax :: IntSet -> Maybe (Key, a)
-- findMin :: IntSet -> (Key, a)
-- findMax :: IntSet -> (Key, a)
-- deleteMin :: IntSet -> IntSet
-- deleteMax :: IntSet -> IntSet


instance Show (IntSet) where
    show m = show $ toList m
