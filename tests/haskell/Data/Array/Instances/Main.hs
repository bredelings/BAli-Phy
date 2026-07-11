{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Classes
import Compiler.Num
import qualified Data.Array as A
import Data.Eq
import qualified Data.Foldable as F
import Data.Functor
import Data.Maybe
import Data.Ord
import Data.Traversable (traverse)
import System.IO (print)

-- Exercise array ordering, display, mapping, folding, and bounds-preserving
-- traversal in element order.
main = do
    let values = A.listArray (2,4) [1,2,3] :: A.Array Int Int
        later = A.listArray (2,4) [1,2,4] :: A.Array Int Int
        shifted = A.listArray (3,5) [1,2,3] :: A.Array Int Int
        empty1 = A.listArray (2,1) [] :: A.Array Int Int
        empty2 = A.listArray (9,8) [] :: A.Array Int Int
        mapped = fmap (+10) values
    print values
    print (values == A.listArray (2,4) [1,2,3])
    print (compare values later == LT)
    print (values == shifted)
    print (empty1 == empty2)
    print (compare empty1 empty2 == EQ)
    print (A.bounds mapped)
    print (A.elems mapped)
    print (F.foldl (+) 0 values)
    print (F.foldr (:) [] values)
    print (traverse (\value -> Just (value + 1)) values
           :: Maybe (A.Array Int Int))
