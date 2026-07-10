{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = print ((2 >< 2) [1, 2, 3] :: Matrix Int)
