{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Confirm membership is checked before a linear array offset is used.
main = do
    let values = A.listArray (-1,1) [10,20,30] :: A.Array Int Int
    print (A.bounds values)
    print (values A.! 2)
