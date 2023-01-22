module Data.IntMap where

import Prelude hiding (map,empty,lookup,(!))
import Data.Functor
import Foreign.Vector

data IntMap e

foreign import bpcall "IntMap:empty" _empty :: () -> IntMap e
empty = _empty ()

foreign import bpcall "IntMap:" size :: IntMap e -> Int

foreign import bpcall "IntMap:has_key" builtin_has_key :: Int -> IntMap e -> Int
has_key key m = case builtin_has_key key m of 1 -> True ; _ -> False

foreign import bpcall "IntMap:subscript" (!) :: IntMap e -> Int -> e

lookup :: Int -> IntMap e -> Maybe e
lookup key m | has_key key m  = Just (m!key)
             | otherwise      = Nothing

foreign import bpcall "IntMap:" erase :: Int -> IntMap e -> IntMap e

foreign import bpcall "IntMap:insert" _insert :: Int -> Int -> IntMap e -> IntMap e
insert (k,v) m = _insert k v m

foreign import bpcall "IntMap:map" map :: (a -> b) -> IntMap a -> IntMap b

instance Functor IntMap where
    fmap = map

fromList []     = empty
fromList (kv:kvs) = insert kv $ fromList kvs

foreign import bpcall "IntMap:keys" _keys :: IntMap e -> EVector Int

keys m = vector_to_list $ _keys m

toList m = [ (k,m!k) | k <- keys m]

instance Show e => Show (IntMap e) where
    show m = show $ toList m
