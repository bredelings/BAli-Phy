{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Immutable update construction must consume the complete association spine.
main = do
    let base = A.listArray (0,0) [1] :: A.Array Int Int
        updated = base A.// ((0,2) : error "update tail was ignored")
    print (A.numElements updated)
