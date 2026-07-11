{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Check that even the final combine result reaches WHNF while the array is
-- constructed, without requiring the updated element to be selected.
main = do
    let original = A.listArray (0,0) [0] :: A.Array Int Int
        accumulated = A.accum
            (\_ _ -> error "final accum result was forced") original [(0,1)]
    print (A.numElements accumulated)
