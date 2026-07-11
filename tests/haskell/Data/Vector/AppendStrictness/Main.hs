{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.Num
import qualified Data.Vector as V
import System.IO (print)

main = print (V.length ((V.empty :: V.Vector Int) V.++
    (error "right append vector was not evaluated" :: V.Vector Int)))
