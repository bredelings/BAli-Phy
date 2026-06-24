{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Bool
import System.IO (print)

main =
  let x = if
      True
      then (1 :: Int)
      else 2
  in print x
