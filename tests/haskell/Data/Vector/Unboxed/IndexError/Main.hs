{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Vector.Unboxed as U
import System.IO (print)

main = print ((U.fromList [10,20] :: U.Vector Int) U.! 2)
