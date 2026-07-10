{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = print (toLists ((2 >< 2) [1, 2, 3, 4, 5] :: Matrix Int))
