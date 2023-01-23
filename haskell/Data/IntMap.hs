module Data.IntMap where

import Prelude hiding (map,empty,lookup,(!))
import Data.Functor
import Foreign.Vector

data IntMap a

type Key = Int

foreign import bpcall "IntMap:empty" _empty :: () -> IntMap a
empty = _empty ()

foreign import bpcall "IntMap:" singleton :: Key -> a -> IntMap a

-- fromSet :: (Key -> a) -> IntSet -> IntMap a

fromList []     = empty
fromList ((k,v):kvs) = insert k v $ fromList kvs

fromListWith f [] = empty
fromListWith f ((k,v):kvs) = insertWith f k v $ fromListWith f kvs

-- FromListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> IntMap a

foreign import bpcall "IntMap:insert" insert :: Key -> a -> IntMap a -> IntMap a

foreign import bpcall "IntMap:" insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a

-- insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a

-- insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> (Maybe a, IntMap a)

foreign import bpcall "IntMap:delete" delete :: Int -> IntMap a -> IntMap a

-- adjust :: (a -> a) -> Key -> IntMap a -> IntMap a

-- adjustWithKey :: (Key -> a -> a) -> Key -> IntMap a -> IntMap a

-- update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap a

-- updateWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a

-- updateLookupWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> (Maybe a, IntMap a)

-- alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a

-- alterF :: Functor f => (Maybe a -> f (Maybe a)) -> Key -> IntMap a -> f (IntMap a)

lookup :: Int -> IntMap a -> Maybe a
lookup key m | member key m  = Just (m!key)
             | otherwise     = Nothing

infixl 9 !?
m !? k = lookup k m

foreign import bpcall "IntMap:subscript" (!) :: IntMap a -> Int -> a

-- We could make a builtin for this
findWithDefault def key m = case lookup key m of
                              Just x  -> x
                              Nothing -> def

foreign import bpcall "IntMap:has_key" builtin_member :: Int -> IntMap a -> Int
member key m = case builtin_member key m of 1 -> True ; _ -> False

notMember k = not . member k

-- lookupLT
-- lookupGT
-- lookupLE
-- lookup GE
            
null m = size m == 0

foreign import bpcall "IntMap:" size :: IntMap a -> Int

foreign import bpcall "IntMap:" union :: IntMap a -> IntMap a -> IntMap a
                                        
foreign import bpcall "IntMap:" unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a

-- foreign import bpcall "IntMap:" unionWithKey :: (Key -> a -> a -> a) > IntMap a -> IntMap a -> IntMap a
                                        
-- unions :: Foldable f => f (IntMap a) -> IntMap a
-- unionsWith :: Foldable f => (a -> a -> a) -> f (IntMap a) -> IntMap a

foreign import bpcall "IntMap:" difference :: IntMap a -> IntMap b -> IntMap a

(\\) = difference

-- differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a 

-- differenceWithKey :: (Key -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a

foreign import bpcall "IntMap:" intersection :: IntMap a -> IntMap b -> IntMap a

foreign import bpcall "IntMap:" intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c

-- intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c

foreign import bpcall "IntMap:disjoint" _disjoint :: IntMap a -> IntMap a -> Int
disjoint m1 m2 = case _disjoint m1 m2 of
                   0 -> False
                   _ -> True

-- compose :: IntMap a -> IntMap Int -> IntMap a


foreign import bpcall "IntMap:map" map :: (a -> b) -> IntMap a -> IntMap b

instance Functor IntMap where
    fmap = map

---

-- Note!  These are supposed be to in ascending order of keys, but are not.
elems m = [ m!k | k <- keys m]

foreign import bpcall "IntMap:keys" _keys :: IntMap a -> EVector Key
keys m = vector_to_list $ _keys m

assocs m = [ (k,m!k) | k <- keys m]

-- keysSet :: IntMap a -> IntSet

toList m = [ (k,m!k) | k <- keys m]

-- filter :: (a -> bool) -> IntMap a -> IntMap a
-- filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
-- restrictKeys :: IntMap a -> IntSet -> IntMap a
-- withoutKeys :: IntMap a -> IntSet -> IntMap a
-- partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
-- partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
-- mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
-- mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b 
-- mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
-- mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c) 
-- split :: Key -> IntMap a -> (IntMap a, IntMap a)
-- splitLookup :: Key -> IntMap a -> (IntMap a, Maybe a, IntMap a) 
-- splitRoot :: IntMap a -> [IntMap a] 

-- isSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
-- isSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
-- isProperSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
-- isProperSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool

-- lookupMin :: IntMap a -> Maybe (Key, a) 
-- lookupMax :: IntMap a -> Maybe (Key, a) 
-- findMin :: IntMap a -> (Key, a) 
-- findMax :: IntMap a -> (Key, a) 
-- deleteMin :: IntMap a -> IntMap a 
-- deleteMax :: IntMap a -> IntMap a 


instance Show a => Show (IntMap a) where
    show m = show $ toList m
