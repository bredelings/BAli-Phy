{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Classes
import Compiler.Num
import qualified Data.Vector as V
import System.IO (print)

main = do
    let values = V.fromList [0,1,2,3,4] :: V.Vector Int
    print (V.toList (V.slice 1 3 values))
    print (V.toList (V.take 3 values))
    print (V.toList (V.take (-1) values))
    print (V.toList (V.drop 2 values))
    print (V.toList (V.drop 20 values))
    print (V.toList (V.take 2 values V.++ V.drop 3 values))
    print (V.toList (V.slice 0 (V.length values) values),
           V.toList (V.take 20 values),
           V.toList (V.drop 0 values))
    print (V.toList (V.empty V.++ values),
           V.toList (values V.++ V.empty),
           V.toList (V.empty V.++ (V.empty :: V.Vector Int)))
