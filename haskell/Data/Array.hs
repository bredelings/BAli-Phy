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

import Data.Bool
import Data.Ix
import Data.List
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

array_to_vector x = list_to_vector (elems x)

instance Functor (Array Int) where
    fmap f arr = mkArray (numElements arr) (\i -> f (arr!i))

instance Foldable (Array Int) where
    toList  = elems
