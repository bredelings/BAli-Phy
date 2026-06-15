{-# LANGUAGE NoImplicitPrelude, RecursiveDo #-}

import Compiler.Num
import Control.Monad (return)
import Control.Monad.Fix (mfix)
import System.IO (print)

main = do
  rec
    x <- return (y + (1 :: Int))
    rec
      y <- return (1 :: Int)
  print (x, y)
