{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Check stored bounds, sizes, and index enumeration without forcing an input
-- list when the requested range is empty.
main = do
    let positive = A.listArray (3,5) [30,40,50] :: A.Array Int Int
        negative = A.listArray (-2,1) [10,20,30,40] :: A.Array Int Int
        empty = A.listArray (2,1) (error "empty array input was forced") :: A.Array Int Int
    print (A.bounds positive)
    print (A.numElements positive)
    print (A.indices positive)
    print (A.bounds negative)
    print (A.numElements negative)
    print (A.indices negative)
    print (A.bounds empty)
    print (A.numElements empty)
    print (A.indices empty)
    print (A.elems empty)
