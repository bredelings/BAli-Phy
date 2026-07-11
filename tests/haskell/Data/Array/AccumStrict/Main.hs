{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import Data.Eq
import qualified Data.Array as A
import System.IO (print)

step _ new = if new == 0 then error "first accumulation result" else new

-- Force every intermediate combining result before processing a later update.
main = do
    let values = A.accumArray step 9 (0,0) [(0,0),(0,1)]
            :: A.Array Int Int
    print (A.numElements values)
