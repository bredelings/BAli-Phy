{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Array construction must inspect the complete association spine even after
-- every backing slot has received a value.
main = do
    let values = A.array (0,0) ((0,10) : error "association tail was ignored")
            :: A.Array Int Int
    print (A.numElements values)
