{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Observe defined fields before selecting the shared undefined-element thunk.
main = do
    let values = A.array (0,2) [(0,10),(2,30)] :: A.Array Int Int
    print (A.numElements values)
    print (values A.! 0)
    print (values A.! 1)
