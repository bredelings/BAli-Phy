{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

!x = 1

main = do
  putStrLn $ show x
