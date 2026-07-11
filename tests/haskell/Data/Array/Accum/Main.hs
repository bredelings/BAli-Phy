{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

step old new = old * 10 + new

-- Process duplicate associations from left to right and retain source bounds.
main = do
    let base = A.listArray (5,5) [1] :: A.Array Int Int
        result = A.accum step base [(5,2),(5,3)]
    print (A.bounds result)
    print (result A.! 5)
