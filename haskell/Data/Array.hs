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

import Data.Ix
import Data.List
import Data.Function
import Compiler.Num

-- hack, until we have a type system
is_array :: a -> Bool
builtin is_array 1 "is_array" "Array"

infixl 9 !
builtin ! 2 "getIndex" "Array"

builtin numElements 1 "arraySize" "Array"
builtin mkArray 2 "mkArray" "Array"

listArray n l = mkArray n (\i -> l !! i)

listArray' l = listArray (length l) l

-- array (0,ix2) list = mkArray ix2 (\i -> find_in_assoc_list list i)

bounds arr = (0,numElements arr-1)

indices = range . bounds

elems   arr = [ arr!ix | ix <- indices arr ]

assocs  arr = [ (ix, arr!ix) | ix <- indices arr ]
