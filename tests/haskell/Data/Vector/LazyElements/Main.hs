{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Classes
import Compiler.Num
import qualified Data.Vector as V
import System.IO (print)

main = do
    let values = V.fromList [7, error "unselected vector element was forced", 9] :: V.Vector Int
        replicated = V.replicate 11 (error "replicated value was forced") :: V.Vector Int
        zero = V.replicate 0 (error "zero-length value was forced") :: V.Vector Int
        full = V.slice 0 (V.length values) values
        leftEmpty = V.empty V.++ values
        rightEmpty = values V.++ V.empty
    print (values V.! 0)
    print (V.length values)
    print (V.toList (V.take 1 values))
    print (V.length replicated)
    print (V.toList zero)
    print (V.length full, full V.! 0, leftEmpty V.! 0, rightEmpty V.! 2)
