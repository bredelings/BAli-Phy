{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Updating one slot must not force unrelated base or replacement values.
main = do
    let base = A.array (0,1) [(0,error "base value was forced"),(1,2)]
            :: A.Array Int Int
        updated = base A.// [(0,error "replacement was forced"),(1,3)]
    print (updated A.! 1)
