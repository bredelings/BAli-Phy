{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Vector as V
import System.IO (print)

main = do
    let values = V.fromList [7, error "unselected vector element was forced", 9] :: V.Vector Int
    print (values V.! 0)
    print (V.length values)
    print (V.toList (V.take 1 values))
