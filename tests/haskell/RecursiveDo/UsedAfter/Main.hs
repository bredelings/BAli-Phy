{-# LANGUAGE NoImplicitPrelude, RecursiveDo #-}

import Compiler.Num
import Control.Monad (return)
import System.IO (print)

main = do
  rec
    x <- return (1 :: Int)
    y <- return (x + 2)
  z <- return (y + 3)
  print (y, z)
