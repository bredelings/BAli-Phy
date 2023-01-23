module Data.IntMap where

import Prelude hiding (map,empty,lookup,(!))
import Data.Functor
import Foreign.Vector

data IntMap a

type Key = Int

foreign import bpcall "IntMap:empty" _empty :: () -> IntMap a
empty = _empty ()

foreign import bpcall "IntMap:" singleton :: Key -> a -> IntMap a

foreign import bpcall "IntMap:" size :: IntMap a -> Int

foreign import bpcall "IntMap:insert" insert :: Key -> a -> IntMap a -> IntMap a

foreign import bpcall "IntMap:" insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
---
foreign import bpcall "IntMap:has_key" builtin_has_key :: Int -> IntMap a -> Int
has_key key m = case builtin_has_key key m of 1 -> True ; _ -> False

foreign import bpcall "IntMap:subscript" (!) :: IntMap a -> Int -> a

lookup :: Int -> IntMap a -> Maybe a
lookup key m | has_key key m  = Just (m!key)
             | otherwise      = Nothing

foreign import bpcall "IntMap:" erase :: Int -> IntMap a -> IntMap a

foreign import bpcall "IntMap:map" map :: (a -> b) -> IntMap a -> IntMap b

instance Functor IntMap where
    fmap = map

fromList []     = empty
fromList ((k,v):kvs) = insert k v $ fromList kvs

foreign import bpcall "IntMap:keys" _keys :: IntMap a -> EVector Key

keys m = vector_to_list $ _keys m

toList m = [ (k,m!k) | k <- keys m]

instance Show a => Show (IntMap a) where
    show m = show $ toList m
