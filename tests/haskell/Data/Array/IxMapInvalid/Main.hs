{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Eq
import qualified Data.Array as A
import System.IO (print)

mapping index = if index == 0 then 0 else 2

-- Report an invalid mapped source index only when selecting that target.
main = do
    let source = A.listArray (0,1) [10,20] :: A.Array Int Int
        mapped = A.ixmap (0,1) mapping source :: A.Array Int Int
    print (mapped A.! 1)
