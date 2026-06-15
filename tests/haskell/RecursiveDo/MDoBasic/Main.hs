{-# LANGUAGE NoImplicitPrelude, RecursiveDo #-}

import Compiler.Num
import Control.Monad (return)
import Control.Monad.Fix (mfix)
import System.IO (print)

main = mdo
  x <- return (y + (1 :: Int))
  y <- return (1 :: Int)
  print x
