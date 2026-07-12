{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import Data.Bool (Bool(False, True))
import qualified Data.Vector.Unboxed as U
import System.IO (print)

boolInt :: Bool -> Int
boolInt False = 0
boolInt True = 1

-- Verify that stable Haskell shape metadata and view construction do not
-- evaluate numeric heads or force the lazy native owner.
main = do
    let values = U.fromList [error "unboxed vector head forced", 2] :: U.Vector Int
    print (U.length values, boolInt (U.null values))
    print (U.length (U.slice 0 1 values))
    print (U.length (U.take 1 values), U.length (U.drop 1 values))
    print (boolInt (U.null (U.slice 1 0 values)))
    print (U.length (U.replicate 0 (error "pair value forced")
                    :: U.Vector (Int,Int)))
    print (U.length (U.replicate 2 (error "nonempty pair value forced")
                    :: U.Vector (Int,Int)))
    print (U.length (U.replicate 1 (error "nested pair value forced")
                    :: U.Vector ((Int,Double),(Int,Int))))
