{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = print (((-1) >< 2) [1, 2] :: Matrix Int)
