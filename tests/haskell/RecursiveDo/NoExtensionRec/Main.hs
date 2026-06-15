{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Control.Monad (return)
import System.IO (print)

main = do
  rec
    x <- return (1 :: Int)
  print x
