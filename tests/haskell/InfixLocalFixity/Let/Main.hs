{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import System.IO (print)

main = print $
  let infixr 5 +++
      x +++ y = x * (10 :: Int) + y
  in (1 :: Int) +++ (2 :: Int) +++ (3 :: Int)
