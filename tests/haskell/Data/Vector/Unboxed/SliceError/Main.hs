{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Vector.Unboxed as U
import System.IO (print)

main = print (U.toList (U.slice 2 2 (U.fromList [10,20,30] :: U.Vector Int)))
