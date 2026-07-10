{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Enum
import Compiler.Num
import Numeric.LinearAlgebra
import System.IO (print)

main = print (toLists ((2 >< 3) [1..] :: Matrix Int))
