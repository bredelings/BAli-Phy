{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = print (((2 >< 3) [1..6] :: Matrix Int) + (2 >< 2) [1..4])
