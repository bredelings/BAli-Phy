{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Check association reordering and verify that a later duplicate replaces an
-- earlier value without forcing the overwritten value.
main = do
    let values = A.array (-2,1)
            [(0,error "overwritten association was forced"),
             (1,40),(-2,10),(-1,20),(0,31)] :: A.Array Int Int
    print (A.bounds values)
    print (A.indices values)
    print (A.elems values)
    print (A.assocs values)
    print (values A.! 0)
