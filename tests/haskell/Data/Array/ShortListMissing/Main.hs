{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- A short listArray input leaves the unfilled suffix undefined until that
-- element is selected.
main = do
    let values = A.listArray (4,6) [10,20] :: A.Array Int Int
    print (values A.! 4)
    print (values A.! 6)
