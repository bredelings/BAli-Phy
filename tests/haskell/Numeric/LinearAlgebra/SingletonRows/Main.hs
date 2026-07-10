{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = do
    print (toLists (fromLists [[1, 2, 3], [4]] :: Matrix Int))
    print (toLists (fromLists [[1.5], [2.5, 3.5]] :: Matrix Double))
