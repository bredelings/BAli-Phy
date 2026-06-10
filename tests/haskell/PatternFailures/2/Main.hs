{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import Data.Maybe
import System.IO (putStrLn)
import Text.Show (show)

main = do
  let f = \(Just x) -> x
  putStrLn $ show $ f Nothing + (1 :: Int)
