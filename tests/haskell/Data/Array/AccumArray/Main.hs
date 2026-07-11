{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Accumulate tuple indices in row-major storage and leave an empty array's
-- initial value and combining function unevaluated.
main = do
    let values = A.accumArray (+) 10 ((0,0),(1,1))
            [((0,1),1),((0,1),2),((1,0),5)]
            :: A.Array (Int,Int) Int
        empty = A.accumArray
            (\_ _ -> error "empty combine was forced")
            (error "empty initial value was forced") (2,1) []
            :: A.Array Int Int
    print (A.elems values)
    print (A.bounds empty)
    print (A.numElements empty)
