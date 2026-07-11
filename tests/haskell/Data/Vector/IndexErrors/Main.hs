{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Compiler.Classes
import qualified Data.Vector as V
import System.IO (print)

main = do
    let values = V.fromList [10,20] :: V.Vector Int
    print (values V.!? (-1))
    print (values V.!? 2)
    print (values V.! 2)
