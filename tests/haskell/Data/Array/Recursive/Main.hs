{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Build a recursive array whose lazy values refer to earlier array elements.
main = do
    let values = A.array (0,3)
            [(0,1),
             (1,(values A.! 0) + 1),
             (2,(values A.! 1) + 1),
             (3,(values A.! 2) + 1)] :: A.Array Int Int
    print (A.elems values)
    print (values A.! 3)
