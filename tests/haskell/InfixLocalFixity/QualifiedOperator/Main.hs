{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Compiler.Fractional
import System.IO (print)

main =
  let infixr 5 /
      x / _ = x
  in print ((20 Compiler.Fractional./ 5 Compiler.Fractional./ 2) :: Double)
