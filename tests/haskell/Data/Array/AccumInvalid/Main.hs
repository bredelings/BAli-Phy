{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Reject an invalid association before forcing its value or the combiner.
main = do
    let base = A.listArray (0,0) [1] :: A.Array Int Int
        values = A.accum (\_ _ -> error "combiner was forced") base
            [(1,error "invalid accumulation value was forced")]
    print (A.numElements values)
