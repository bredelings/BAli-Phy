{-# LANGUAGE NoImplicitPrelude #-}
module Data.Array (module Data.Array,
                   module Data.Ix)
    where

-- See GHC/Arr.hs for some implementation details
-- In Data.Array         - Basic non-strict arrays (interface, I think)
-- See Data.Array.MArray - Mutable arrays   (interface)
-- See Data.Array.IArray - Immutable arrays (interfaced)
-- See Data.Array.ST     - Mutable boxed and unboxed array in the ST monad.
-- See Data.Array.IO     - Mutable boxed and unboxed array in the IO monad.
-- See Data.Array.Unboxed

-- Possibly Data.Array is not going to be as fast as Data.Vector:
--   Data.Vector is 0-indexed.   
--   Data.Vector.fromList might be what I want instead of listArray.
--   Data.Vector has a "generate" function that is equivalent to my mkArray      


import Compiler.Base -- for `seq`
import Data.Bool
import Data.Maybe
import Data.Ix
import Data.List
import Data.Ord
import Data.Function
import Compiler.Num
import Foreign.Vector
import Data.Functor
import Data.Foldable
import Text.Show

data Array a b

infixl 9 !
foreign import bpcall "Array:getIndex" (!) :: Array a b -> a -> b

foreign import bpcall "Array:arraySize" numElements :: Array a b -> Int
foreign import bpcall "Array:mkArray" mkArray :: a -> (a -> b) -> Array a b

foreign import bpcall "Array:" removeElement :: Int -> Array Int e -> Array Int e

-- array :: Ix i => (i,i) -> [(i,e)] -> Array i e

{- This takes linear time to set each element of the array
-- I guess we could convert the list to [(Int,e)] -> EVector (EPair Int (Pointer e))?
-- Then we could construct the array.
-- But how to ensure that the (Pointer e) counts as a GC reference?
-}

-- listArray :: Ix i => (i, i) -> [e] -> Array i e 
listArray n l = mkArray n (\i -> l !! i)

listArray' l = listArray (length l) l

-- accumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(i,a)] -> Array i e

-- (!) :: Ix i => Array i e -> i -> e infixl 9 

-- bounds :: Array i e -> (i, i) 
bounds arr = (0,numElements arr-1)

-- indices :: Ix i => Array i e -> [i] 
indices = range . bounds

-- elems :: Array i e -> [e] 
elems   arr = [ arr!ix | ix <- indices arr ]

-- assocs :: Ix i => Array i e -> [(i, e)] 

assocs  arr = [ (ix, arr!ix) | ix <- indices arr ]

array_to_vector x = toVector (elems x)
vectorToArray v = mkArray (vector_size v) (get_vector_index v)

instance Functor (Array Int) where
    fmap f arr = mkArray (numElements arr) (\i -> f (arr!i))

instance Foldable (Array Int) where
    toList  = elems
    length = numElements

    foldl f z arr = go 0 z where
        go i x | i < n      = go (i+1) (f x (arr!i))
               | otherwise  = x
        n = length arr

    foldl' f z arr = go 0 z where
        go i x | i < n      = let z' = (f x (arr!i)) in z' `seq` go (i+1) z'
               | otherwise  = x
        n = length arr

    foldl1 f arr = go 1 (arr!0) where
        go i x | i < n      = let z' = (f x (arr!i)) in z' `seq` go (i+1) z'
               | otherwise  = x
        n = length arr

    foldr f z arr = go 0 where
        go i | i < n     = f (arr!i) $ go (i+1)
             | otherwise = z
        n = length arr

    foldr1 f arr = go 0 where
        go i | i < n' = f (arr!i) $ go (i+1)
             | otherwise =  (arr!n')
        n' = length arr - 1

elemIndexArray val array = go 0 where
    go i | i >= n            = Nothing
         | (array!i) == val  = Just i
         | otherwise         = go (i+1)
    n = numElements array

mapnA n f arr = mkArray n (\i -> f $ arr!i)


instance Show i => Show (Array Int i) where
    show a = show (toList a)


-- (//) :: Ix i => Array i e -> [(i, e)] -> Array i e infixl 9


-- accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e 


-- ixmap :: (Ix i, Ix j) => (i, i) -> (i -> j) -> Array j e -> Array i e 
