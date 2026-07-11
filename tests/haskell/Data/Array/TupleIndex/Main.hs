{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Enum
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Check that tuple ranges enumerate and index in row-major order.
main = do
    let values = A.listArray ((1,-1),(2,1)) [10..15]
            :: A.Array (Int,Int) Int
    print (A.bounds values)
    print (A.indices values)
    print (A.elems values)
    print (values A.! (1,1))
    print (values A.! (2,-1))
    print (A.assocs values)
