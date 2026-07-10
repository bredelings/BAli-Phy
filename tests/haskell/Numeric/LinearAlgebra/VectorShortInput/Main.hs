{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = print ((3 |> [1, 2]) :: Vector Int)
