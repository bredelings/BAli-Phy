{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Reject a tuple outside one component even when its raw linear offset would
-- alias an in-range element of the backing vector.
main = do
    let values = A.listArray ((0,0),(1,1)) [10,20,30,40]
            :: A.Array (Int,Int) Int
    print (values A.! (0,2))
