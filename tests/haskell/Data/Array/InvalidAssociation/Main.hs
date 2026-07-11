{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import System.IO (print)

-- Force array construction so an out-of-range association is rejected while
-- its element value remains irrelevant.
main = do
    let values = A.array (0,1)
            [(0,10),(2,error "invalid association value was forced")]
            :: A.Array Int Int
    print (values A.! 0)
