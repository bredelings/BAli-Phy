{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Control.Monad (return)
import Control.Monad.Fix (mfix)
import System.IO (print)

main = mdo
  x <- return (1 :: Int)
  print x
