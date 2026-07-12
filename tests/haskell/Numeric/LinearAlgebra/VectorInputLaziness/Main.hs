{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = do
    print (toList ((0 |> error "zero-length input was evaluated") :: Vector Int))
    print (toList ((1 |> (7 : error "excess input tail was evaluated")) :: Vector Int))
