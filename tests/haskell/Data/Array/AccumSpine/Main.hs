{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Accumulation must consume the complete association spine after updating.
main = do
    let values = A.accumArray (+) 0 (0,0)
            ((0,1) : error "accumulation tail was ignored")
            :: A.Array Int Int
    print (A.numElements values)
