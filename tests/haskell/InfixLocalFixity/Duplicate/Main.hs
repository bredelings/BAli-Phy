{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import System.IO (print)

main =
  let infixl 5 +++
      infixr 5 +++
      x +++ y = x + y
  in print (((1 :: Int) +++ (2 :: Int)) +++ (3 :: Int))
