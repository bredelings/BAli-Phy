module Data.IntSet where

import Prelude hiding (map,empty,elems,filter)
import Data.Functor
import qualified Data.List as L
import Foreign.Vector
import Data.Foldable (foldr)
import Data.Array (vectorToArray)
import Control.DeepSeq

data IntSet

type Key = Int

foreign import ecall "IntSet:" empty :: IntSet

foreign import ecall "IntSet:" singleton :: Key -> IntSet

-- Should this be more efficient? With immer it might be OK, but maybe we should
-- convert to an EVector Int, and then convert that to an IntSet
fromList []     = empty
fromList (k:ks) = insert k $ fromList ks

foreign import bpcall "IntSet:" insert :: Key -> IntSet -> IntSet

foreign import bpcall "IntSet:" delete :: Key -> IntSet -> IntSet

foreign import ecall "IntSet:" member :: Int -> IntSet -> Bool

notMember k = not . member k

-- lookupLT
-- lookupGT
-- lookupLE
-- lookup GE

null m = size m == 0

foreign import ecall "IntSet:" size :: IntSet -> Int

foreign import bpcall "IntSet:" union :: IntSet -> IntSet -> IntSet

unions :: Foldable f => f (IntSet) -> IntSet
unions sets = foldr union empty sets

foreign import bpcall "IntSet:" difference :: IntSet -> IntSet -> IntSet

(\\) = difference

foreign import bpcall "IntSet:" isSubsetOf :: IntSet -> IntSet -> Bool

isProperSubsetOf :: IntSet -> IntSet -> Bool
isProperSubsetOf s1 s2 = (s1 `isSubsetOf` s2) && (size s1 < size s2)

foreign import bpcall "IntSet:" intersection :: IntSet -> IntSet -> IntSet

foreign import bpcall "IntSet:" disjoint :: IntSet -> IntSet -> Int

-- Note!  These are supposed be to in ascending order of keys, but are not.

foreign import bpcall "IntSet:keys" _keys :: IntSet -> EVector Key
elems m = vectorToList $ _keys m

toList m = elems m

toArray s = vectorToArray $ _keys s

toAscList m = toList m

toDescList m = toList m

filter :: (Key -> Bool) -> IntSet -> IntSet
filter p set = fromList $ L.filter p $ toList set

-- partition :: (Key -> Bool) -> IntSet -> (IntSet, IntSet)
-- split :: Key -> IntSet -> (IntSet, IntSet)
-- splitMember :: Key -> IntSet -> (IntSet, Bool, IntSet)
-- splitRoot :: IntSet -> [IntSet]

map :: (Key -> Key) -> IntSet -> IntSet
map f set = fromList $ L.map f $ toList set
-- mapMonotonic :: (Key -> Key) -> IntSet -> IntSet

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

foreign import bpcall "IntSet:" mapNegate :: IntSet -> IntSet

instance NFData IntSet

