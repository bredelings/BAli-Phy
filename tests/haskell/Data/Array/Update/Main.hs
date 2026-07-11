{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Check bounds preservation, immutability, tuple updates, and last-write-wins
-- replacement without forcing an overwritten value.
main = do
    let base = A.listArray (2,4) [10,20,30] :: A.Array Int Int
        updated = base A.// [(3,error "overwritten update was forced"),(2,11),(3,31)]
        tupleBase = A.listArray ((0,0),(1,1)) [1,2,3,4]
                :: A.Array (Int,Int) Int
        tupleUpdated = tupleBase A.// [((0,1),20)]
        empty = (A.listArray (2,1) [] :: A.Array Int Int) A.// []
    print (A.bounds updated)
    print (A.elems updated)
    print (A.elems base)
    print (A.elems tupleUpdated)
    print (A.bounds empty)
