{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Map one source array into tuple target order and avoid evaluating a mapping
-- function or source array when the target bounds are empty.
main = do
    let source = A.listArray (0,3) [10,20,30,40] :: A.Array Int Int
        mapped = A.ixmap ((0,0),(1,1)) (\(row,column) -> row * 2 + column) source
        empty = A.ixmap (2,1)
            (error "empty mapping was forced" :: Int -> Int)
            (error "empty source was forced" :: A.Array Int Int)
            :: A.Array Int Int
    print (A.bounds mapped)
    print (A.assocs mapped)
    print (A.bounds empty)
    print (A.numElements empty)
