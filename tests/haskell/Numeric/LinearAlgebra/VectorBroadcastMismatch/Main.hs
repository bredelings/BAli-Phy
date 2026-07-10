{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = print ((fromList [1, 2] :: Vector Int) + fromList [3, 4, 5])
