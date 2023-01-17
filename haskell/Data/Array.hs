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

import Compiler.Base -- for `seq`
import Data.Bool
import Data.Ix
import Data.List
import Data.Ord
import Data.Function
import Compiler.Num
import Foreign.Vector
import Data.Functor
import Data.Foldable

data Array a b

infixl 9 !
foreign import bpcall "Array:getIndex" (!) :: Array a b -> a -> b

foreign import bpcall "Array:arraySize" numElements :: Array a b -> Int
foreign import bpcall "Array:mkArray" mkArray :: a -> (a -> b) -> Array a b

listArray n l = mkArray n (\i -> l !! i)

listArray' l = listArray (length l) l

-- array (0,ix2) list = mkArray ix2 (\i -> find_in_assoc_list list i)

bounds arr = (0,numElements arr-1)

indices = range . bounds

elems   arr = [ arr!ix | ix <- indices arr ]

assocs  arr = [ (ix, arr!ix) | ix <- indices arr ]

to_vector = list_to_vector . toList
array_to_vector x = list_to_vector (elems x)

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

    foldr f z arr = go 0 where
        go i | i < n     = f (arr!i) $ go (i+1)
             | otherwise = z
        n = length arr

    foldr1 f arr = go 0 where
        go i | i < n' = f (arr!i) $ go (i+1)
             | otherwise =  (arr!n')
        n' = length arr - 1
