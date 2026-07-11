{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Check short, excess, and empty list inputs while observing only elements
-- that are defined by the supplied prefix.
main = do
    let short = A.listArray (-1,1) [4,5] :: A.Array Int Int
        excess = A.listArray (5,6)
            [10,20,error "excess listArray element was forced"] :: A.Array Int Int
        empty = A.listArray (1,0)
            (error "empty listArray input was forced") :: A.Array Int Int
    print (A.bounds short)
    print (A.numElements short)
    print (short A.! (-1))
    print (short A.! 0)
    print (A.elems excess)
    print (A.elems empty)
