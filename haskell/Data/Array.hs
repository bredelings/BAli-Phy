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

data Array a b

-- hack, until we have a type system
is_array :: a -> Bool
builtin is_array 1 "Array:is_array"

infixl 9 !
builtin (!) 2 "Array:getIndex"

builtin numElements 1 "Array:arraySize"
builtin mkArray 2 "Array:mkArray"

listArray n l = mkArray n (\i -> l !! i)

listArray' l = listArray (length l) l

-- array (0,ix2) list = mkArray ix2 (\i -> find_in_assoc_list list i)

bounds arr = (0,numElements arr-1)

indices = range . bounds

elems   arr = [ arr!ix | ix <- indices arr ]

assocs  arr = [ (ix, arr!ix) | ix <- indices arr ]

arrayMap f arr = mkArray (numElements arr) (\i -> f (arr!i))

array_to_vector x = list_to_vector (elems x)
