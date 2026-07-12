{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Vector.Unboxed as U
import System.IO (print)

main = print (U.length (U.replicate (-1)
    (error "negative replicate value was forced") :: U.Vector Int))
