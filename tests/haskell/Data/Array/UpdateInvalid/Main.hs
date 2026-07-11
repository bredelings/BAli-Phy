{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Reject an invalid update index before its lazy replacement is evaluated.
main = do
    let base = A.listArray (0,1) [1,2] :: A.Array Int Int
        updated = base A.// [(2,error "invalid update value was forced")]
    print (A.numElements updated)
