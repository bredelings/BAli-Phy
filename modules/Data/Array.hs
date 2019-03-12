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

infixl 9 !
builtin ! 2 "getIndex" "Array"

builtin arraySize 1 "arraySize" "Array"
builtin mkArray 2 "mkArray" "Array"

listArray n l = mkArray n (\i -> l !! i)

listArray' l = listArray (length l) l

--

