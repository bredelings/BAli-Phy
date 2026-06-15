{-# LANGUAGE NoImplicitPrelude, RecursiveDo #-}

import Compiler.Num
import Control.Monad (return)
import System.IO (print)

main = mdo
  x <- return (1 :: Int)
  y <- return (x + (1 :: Int))
  print y
