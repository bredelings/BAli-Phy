{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Vector as V
import System.IO (print)

main = print (V.length (V.replicate (-1)
    (error "negative replicate value was forced") :: V.Vector Int))
