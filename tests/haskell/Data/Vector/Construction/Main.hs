{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Vector as V
import System.IO (print)

main = do
    print (V.toList (V.empty :: V.Vector Int))
    print (V.toList (V.singleton 4 :: V.Vector Int))
    print (V.toList (V.replicate 3 7 :: V.Vector Int))
    print (V.toList (V.replicate 11 8 :: V.Vector Int))
    print (V.toList (V.fromList [0,1,2,3,4,5,6,7,8,9] :: V.Vector Int))
    print (V.toList (V.fromList [0,1,2,3,4,5,6,7,8,9,10] :: V.Vector Int))
    print (V.length (V.fromList [10,20,30] :: V.Vector Int))
