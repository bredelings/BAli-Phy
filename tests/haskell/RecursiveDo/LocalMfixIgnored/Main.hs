{-# LANGUAGE NoImplicitPrelude, RecursiveDo #-}

import Compiler.Num
import Control.Monad (return)
import System.IO (print)

mfix = 1 :: Int

main = do
  rec
    x <- return (y + (1 :: Int))
    y <- return (1 :: Int)
  print (mfix, x)
